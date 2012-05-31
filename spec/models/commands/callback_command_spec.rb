require 'spec_helper'

module Commands
  describe CallbackCommand do
    let(:url) { 'http://www.example.com' }

    before(:each) do
      @session = Session.new
      @session.pbx = mock('pbx')
      @session.pbx.should_receive(:caller_id).and_return('999')
      @session.channel = mock('channel')
      @session.channel.should_receive(:name).and_return('foo')
      @session.call_flow = CallFlow.make
      @default_body = {:From => '999', :Channel => 'foo'}
      @session.call_log = CallLog.make
      apply_call_flow
    end

    def assert_log(options = {})
      options[:method] ||= "post"
      options[:trace_params] ||= "CallSid=#{@session.call_id}&Channel=foo&From=999"
      @session.should_receive(:log).with({
        :info => "Callback #{options[:method]} #{url}",
        :trace => "Callback #{options[:method]} #{url} with #{options[:trace_params]}"
      })
    end

    def authenticated_url(user, password)
      uri = URI.parse(url)
      uri.user = user
      uri.password = password
      uri.to_s
    end

    def apply_call_flow(user = "", password = "")
      call_flow = @session.call_flow
      call_flow.callback_url_user = user
      call_flow.callback_url_password = password
      call_flow.save
      @session.call_flow = call_flow
    end

    def expect_callback(method, url, options = {})
      with = options[:with] || {}
      callback_options = {}
      callback_options[:params] = with[:body]
      callback_options[:method] = method
      callback_options[:authentication] = with[:authentication]

      response_header = double('response_header')
      http = double('http', :response_header => response_header)

      response_header.stub(:[]).with(EventMachine::HttpClient::CONTENT_TYPE).and_return(options[:content_type])
      http.stub(:response).and_return(options[:and_return])

      @command.should_receive(:callback).with(url, callback_options).and_return(http)
    end

    it "run with url as string" do
      @command = CallbackCommand.new(url)

      assert_log

      @session.should_receive(:trace).with("Callback returned application/xml: <Response><Hangup/></Response>")

      expect_callback :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml'

      @command.run(@session).should eq(Commands::HangupCommand.new)
    end

    context "running with an app which has http basic authentication for the callback url", :focus => true do
      before do
        assert_log
        apply_call_flow("user", "password")
      end

      context "and the url does not contain any http basic authentication" do
        it "should use the apps configured http basic authentication" do
          @command = CallbackCommand.new(url)

          expect_callback :post, url, :with => {:authentication => {:user => 'user', :password => 'password'}, :body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml'

          @command.run @session
        end
      end

      context "and the url already contains http basic authentication" do
        it "should use the urls http basic authentication" do
          @command = CallbackCommand.new(authenticated_url("url_user", "url_password"))

          expect_callback :post, url, :with => {:authentication => {:user => 'url_user', :password => 'url_password'}, :body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml'

          @command.run @session
        end
      end
    end

    it "run with url and get method" do
      @command = CallbackCommand.new(url, :method => :get)

      assert_log(:method => :get)

      @session.should_receive(:trace).with("Callback returned application/xml: <Response><Hangup/></Response>")

      expect_callback :get, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml'

      @command.run(@session).should eq(Commands::HangupCommand.new)
    end

    it "run without url" do
      @command = CallbackCommand.new

      assert_log

      @session.should_receive(:callback_url).and_return(url)
      @session.should_receive(:trace).with("Callback returned application/xml: <Response><Hangup/></Response>")

      expect_callback :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml'

      @command.run(@session).should eq(Commands::HangupCommand.new)
    end

    it "run receives json in response" do
      @command = CallbackCommand.new(url)

      assert_log

      @session.should_receive(:trace).with("Callback returned application/json: hangup();")

      expect_callback :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => 'hangup();', :content_type => 'application/json'

      @command.run(@session).should eq(Commands::JsCommand.new('hangup();'))
    end

    it "run with custom parameters" do
      @command = CallbackCommand.new(url, :params => {:Digits => :digits})

      assert_log(:trace_params => "CallSid=#{@session.call_id}&Channel=foo&Digits=123&From=999")
      @session[:digits] = '123'
      @session.should_receive(:trace).with("Callback returned application/xml: <Response><Hangup/></Response>")

      expect_callback :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id, :Digits => '123')}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml'

      @command.run(@session).should eq(Commands::HangupCommand.new)
    end

    it "continues with following commands after callback result" do
      @command = Compiler.make do
        Callback()
        Hangup()
      end

      assert_log

      @session.should_receive(:callback_url).and_return(url)
      @session.should_receive(:trace).with("Callback returned application/xml: <Response><Pause/></Response>")

      expect_callback :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Pause/></Response>', :content_type => 'application/xml'

      @command.run(@session).should eq(Compiler.make { Pause(); Hangup() })
    end
  end
end