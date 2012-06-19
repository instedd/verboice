require 'spec_helper'
require 'webmock/rspec'
require "rspec/expectations"

ignore CallFlow do

  API_URL = /.*www.google.com\/fusiontables\/api.*/

  let!(:call_log) do
    CallLog.make :call_flow => call_flow,
      :address => "5551000",
      :started_at => DateTime.new(2012,1,1,8),
      :finished_at => DateTime.new(2012,1,1,9),
      :traces => [
        Trace.new(call_flow_id: call_flow.id, result: "Played", step_name: "Play Step", step_id: "1")
      ]
  end

  let(:oauth_token) { OAuthToken.make :service => :google, :access_token => "ACCESS" }
  let(:account)     { Account.make :google_oauth_token => oauth_token }
  let(:project)     { Project.make :account => account }
  let(:channel)     { Channel.make :account => account }

  let(:call_flow) do
    CallFlow.make :project => project,
      :account => account,
      :fusion_table_name => "my_table",
      :user_flow => [{
        'id' => 1,
        'root' => 1,
        'type' => 'play',
        'name' => 'Play Step',
        'message' => { "name" => "Some explanation message", "type" => "text" }
      }]
  end

  before(:each) do
    Timecop.freeze(DateTime.new(2012,1,1,10,0,0))
  end

  after(:each) do
    Timecop.return
  end

  def get_columns(opts={})
    id = opts[:id]
    result = opts[:result]
    stub_request(:get, API_URL)
      .with(:query => {"sql" => "DESCRIBE #{id}", "access_token" => (opts[:access_token] || "ACCESS")})
      .to_return(:body => "column id,name,type\n" + result.map{|row| row.join(',')}.join("\n"))
  end

  def get_tables(opts={})
    id, name = opts[:result]
    res_csv = id && name ? "\n#{id},#{name}" : ""
    stub_request(:get, "https://www.google.com/fusiontables/api/query")
      .with(:query => {"sql" => "SHOW TABLES", "access_token" => "ACCESS"})
      .to_return(:body => "table id,name" + res_csv)
  end

  def post_create_table(opts={})
    sql = opts[:sql]
    id = opts[:result]
    stub_request(:post, "https://foowww.google.com/fusiontables/api/query")
      .with(:body => {"sql" => sql})
      .to_return(:body => "tableid\n#{id}")
  end

  def post_data(opts={})
    sql = opts[:sql]
    if sql
      stub_request(:post, API_URL).with(:body => {"sql" => sql})
    else
      stub_request(:post, API_URL)
    end
  end

  context "refresh token" do

    CREATE_TABLE_SQL = "CREATE TABLE my_table_%s ('Call ID':STRING, 'Phone Number':STRING, 'State':STRING, 'Start Time':DATETIME, 'End Time':DATETIME, 'Play Step':STRING)"

    before(:each) do
      stub_request(:any, API_URL).to_return(:status => 500)
    end

    it "should have google oauth token" do
      account.google_oauth_token.should eq(oauth_token)
    end

    it "should refresh access token if expired" do
      token = call_flow.account.google_oauth_token
      token.expires_at = 1.hour.ago
      token.save!
      token.should_receive(:refresh!).and_return(true)

      puts "Oauth token obj id #{token.object_id}"

      expect {
        call_flow.push_results(call_log)
      }.to raise_error
    end

    it "should not refresh access token if not expired" do
      token = call_flow.account.google_oauth_token
      token.expires_at = DateTime.now + 1.hour
      token.save!
      token.should_not_receive(:refresh!)

      expect {
        call_flow.push_results(call_log)
      }.to raise_error
    end

  end

  context "table creation" do

    before(:each) do
      post_data # Stub upload information
    end

    it "should create table if first push" do
      call_flow.current_fusion_table_id = nil
      tables = get_tables(:result => :empty)
      create = post_create_table(:result => "2000", :sql => CREATE_TABLE_SQL % "001"),

      call_flow.push_results(call_log)

      call_flow.reload.current_fusion_table_id.should eq("2000")
      create.should have_been_made
    end


    it "should create table if it did not exist" do
      call_flow.current_fusion_table_id = "1000"
      tables = get_tables(:result => :empty)
      create = post_create_table(:result => "2000", :sql => CREATE_TABLE_SQL% "001")

      call_flow.push_results(call_log)

      call_flow.current_fusion_table_id.should eq("2000")
      tables.should have_been_made
      create.should have_been_made
    end


    it "should not create table if it exists" do
      call_flow.current_fusion_table_id = "1000"
      tables = get_tables(:result => ["my_table", "1000"]),
      cols = get_columns(:id => "1000", :result => [
        ["col0", "Call ID", "string"],
        ["col1", "Phone Number", "string"],
        ["col2", "State", "string"],
        ["col3", "Start Time", "datetime"],
        ["col4", "End Time", "datetime"],
        ["col5", "Play Step", "string"]
      ])

      call_flow.push_results(call_log)

      call_flow.current_fusion_table_id.should eq("1000")
      tables.should have_been_made
      cols.should have_been_made
    end

    it "should not create table if there are extra columns" do
      call_flow.current_fusion_table_id = "1000"
      tables = get_tables(:result => ["my_table", "1000"])
      cols = get_columns(:id => "1000", :result => [
        ["col0", "Call ID", "string"],
        ["col1", "Phone Number", "string"],
        ["col2", "State", "string"],
        ["col3", "Start Time", "datetime"],
        ["col4", "End Time", "datetime"],
        ["col5", "Play Step", "string"],
        ["col6", "Additional data", "string"]
      ])

      call_flow.push_results(call_log)

      call_flow.current_fusion_table_id.should eq("1000")
      tables.should have_been_made
      cols.should have_been_made
    end

    it "should create table if columns are missing" do
      call_flow.current_fusion_table_id = "1000"
      tables = get_tables(:result => ["my_table", "1000"]),
      cols = get_columns(:id => "1000", :result => [
        ["col0", "Call ID", "string"],
        ["col1", "Phone Number", "string"],
        ["col2", "State", "string"],
        ["col3", "Start Time", "datetime"],
        ["col4", "End Time", "datetime"]
      ])
      create = post_create_table(:result => "2000", :sql => CREATE_TABLE_SQL % "002")

      call_flow.push_results(call_log)

      call_flow.current_fusion_table_id.should eq("2000")
      tables.should have_been_made
      cols.should   have_been_made
      create.should have_been_made
    end

    it "should create table if columns do not match" do
      call_flow.current_fusion_table_id = "1000"
      tables = get_tables(:result => ["my_table", "1000"]),
      cols = get_columns(:id => "1000", :result => [
        ["col0", "Call ID", "string"],
        ["col1", "Phone Number", "string"],
        ["col2", "State", "string"],
        ["col3", "Start Time", "datetime"],
        ["col4", "End Time", "datetime"],
        ["col5", "Different step", "string"],
      ])
      create = post_create_table(:result => "2000", :sql => CREATE_TABLE_SQL % "002")

      call_flow.push_results(call_log)

      call_flow.current_fusion_table_id.should eq("2000")
      tables.should have_been_made
      cols.should   have_been_made
      create.should have_been_made
    end

    it "should create table using unique column names" do
      call_flow.user_flow += [{
        'id' => 120,
        'type' => 'play',
        'name' => 'Play Step',
        'message' => { "name" => "Some explanation message", "type" => "text" }
      }]
      call_flow.save!

      create = post_create_table(:result => "2000", :sql => "CREATE TABLE my_table_001 ('Call ID':STRING, 'Phone Number':STRING, 'State':STRING, 'Start Time':DATETIME, 'End Time':DATETIME, 'Play Step 1':STRING, 'Play Step 102':STRING)")

      call_flow.push_results(call_log)
      call_flow.current_fusion_table_id.should eq("2000")

      create.should have_been_made
    end

  end

  context "upload data" do

    it "should upload call log data" do
      call_flow.current_fusion_table_id = nil
      create = post_create_table(:result => "2000", :sql => CREATE_TABLE_SQL % "001")
      upload = post_data(:sql => "INSERT INTO 2000 ('Call ID', 'Phone Number', 'State', 'Start Time', 'End Time', 'Play Step 1') VALUES ('#{call_log.id}', '5551000', 'completed', '#{DateTime.new(2012,1,1,8)}', '#{DateTime.new(2012,1,1,8)}', 'Played')")

      call_flow.push_results(call_log)

      create.should have_been_made
      upload.should have_been_made
    end

  end

end