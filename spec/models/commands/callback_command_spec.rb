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

    it "run with url as string" do
      assert_log

      @session.should_receive(:trace).with("Callback returned application/xml: <Response><Hangup/></Response>")

      result = expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
        CallbackCommand.new(url).run @session
      end
      result.should == Commands::HangupCommand.new
    end

    it "interpolates url with session variables" do
      url = 'http://www.domain.com/{foo}?key={bar}'
      interpolated_url = 'http://www.domain.com/the_foo?key=the_bar'
      @session.should_receive(:[]).with('foo').and_return('the_foo')
      @session.should_receive(:[]).with('bar').and_return('the_bar')

      expect_em_http :post, interpolated_url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
        CallbackCommand.new(url).run @session
      end
    end

    context "running with an app which has http basic authentication for the callback url", :focus => true do
      before do
        assert_log
        apply_call_flow("user", "password")
      end

      context "and the url does not contain any http basic authentication" do
        it "should use the apps configured http basic authentication" do
          expect_em_http :post, url, :with => {:head => {'authorization' => ['user', 'password']}, :body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
            CallbackCommand.new(url).run @session
          end
        end
      end

      context "and the url already contains http basic authentication" do
        it "should use the urls http basic authentication" do
          expect_em_http :post, url, :with => {:head => {'authorization' => ['url_user', 'url_password']}, :body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
            CallbackCommand.new(authenticated_url("url_user", "url_password")).run @session
          end
        end
      end
    end

    it "run with url and get method" do
      assert_log(:method => :get)

      @session.should_receive(:trace).with("Callback returned application/xml: <Response><Hangup/></Response>")

      result = expect_em_http :get, url, :with => @default_body.merge(:CallSid => @session.call_id), :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
        CallbackCommand.new(url, :method => :get).run(@session)
      end
      result.should == Commands::HangupCommand.new
    end

    it "run without url" do
      assert_log

      @session.should_receive(:callback_url).and_return(url)
      @session.should_receive(:trace).with("Callback returned application/xml: <Response><Hangup/></Response>")

      result = expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
        CallbackCommand.new.run @session
      end
      result.should == Commands::HangupCommand.new
    end

    it "run receives json in response" do
      assert_log

      @session.should_receive(:trace).with("Callback returned application/json: hangup();")

      result = expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => 'hangup();', :content_type => 'application/json' do
        CallbackCommand.new(url).run @session
      end
      result.should == Commands::JsCommand.new('hangup();')
    end

    it "run with custom parameters" do
      assert_log(:trace_params => "CallSid=#{@session.call_id}&Channel=foo&Digits=123&From=999")
      @session[:digits] = '123'
      @session.should_receive(:trace).with("Callback returned application/xml: <Response><Hangup/></Response>")

      result = expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id, :Digits => '123')}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
        CallbackCommand.new(url, :params => {:Digits => :digits}).run @session
      end
      result.should == Commands::HangupCommand.new
    end

    it "continues with following commands after callback result" do
      assert_log

      @session.should_receive(:callback_url).and_return(url)
      @session.should_receive(:trace).with("Callback returned application/xml: <Response><Pause/></Response>")

      result = expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Pause/></Response>', :content_type => 'application/xml' do
        Compiler.make do
          Callback()
          Hangup()
        end.run @session
      end
      result.should == Compiler.make { Pause(); Hangup() }
    end

    describe "variables" do
      let(:options) { {:response_type => :variables}}
      let(:variables) { {:key1 => 'value1', :key2 => 'value2'} }

      before(:each) do
        assert_log
        @session.should_receive(:trace).with("Callback returned application/json: #{variables.to_json}")
      end

      it "assigns and persists returned variables" do
        result = expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => variables.to_json, :content_type => 'application/json' do
          CallbackCommand.new(url, options).run @session
        end

        expected = Compiler.make do
          Assign 'key1', 'value1'
          PersistVariable 'key1', 'value1'
          Assign 'key2', 'value2'
          PersistVariable 'key2', 'value2'
        end

        result.should eq(expected)
      end

      it "continues with the following commands after the assigns and persists commands" do
        result = expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => variables.to_json, :content_type => 'application/json' do
          Compiler.make do |c|
            c.Callback url, options
            c.Hangup
          end.run @session
        end

        expected = Compiler.make do
          Assign 'key1', 'value1'
          PersistVariable 'key1', 'value1'
          Assign 'key2', 'value2'
          PersistVariable 'key2', 'value2'
          Hangup()
        end

        result.should eq(expected)
      end
    end

    describe "none" do
      let(:options) { {:response_type => :none}}

      before(:each) do
        assert_log
        @session.should_receive(:trace).with("Callback returned text/plain: ")
      end

      it "continues with the next command" do
        result = expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '', :content_type => 'text/plain' do
          Compiler.make do |c|
            c.Callback url, options
            c.Hangup
          end.run @session
        end

        result.should eq(Commands::HangupCommand.new)
      end
    end

  end
end