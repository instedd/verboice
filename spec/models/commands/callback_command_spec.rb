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
      @session.application = mock(
        'application', :callback_url_user => nil, :callback_url_password => nil
      ).as_null_object
      @session.stub(:push_commands).with([:hangup])
      @default_body = {:From => '999', :Channel => 'foo'}
      @session.call_log = CallLog.make
      apply_application
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

    def apply_application(user = "", password = "")
      app = @session.call_log.channel.application
      app.callback_url_user = user
      app.callback_url_password = password
      app.save
      @session.application = app
    end

    it "run with url as string" do
      assert_log

      @session.should_receive(:trace).with("Callback returned application/xml: <Response><Hangup/></Response>")
      @session.should_receive(:push_commands).with([:hangup])

      expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
        CallbackCommand.new(url).run @session
      end
    end

    context "running with an app which has http basic authentication for the callback url", :focus => true do
      before do
        assert_log
        apply_application("user", "password")
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
      @session.should_receive(:push_commands).with([:hangup])

      expect_em_http :get, url, :with => @default_body.merge(:CallSid => @session.call_id), :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
        CallbackCommand.new(url, :method => :get).run @session
      end
    end

    it "run without url" do
      assert_log

      @session.should_receive(:callback_url).and_return(url)
      @session.should_receive(:trace).with("Callback returned application/xml: <Response><Hangup/></Response>")
      @session.should_receive(:push_commands).with([:hangup])

      expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
        CallbackCommand.new.run @session
      end
    end

    it "run receives json in response" do
      assert_log

      @session.should_receive(:trace).with("Callback returned application/json: hangup();")
      @session.should_receive(:push_commands).with([:js => 'hangup();'])

      expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => 'hangup();', :content_type => 'application/json' do
        CallbackCommand.new(url).run @session
      end
    end

    it "run with custom parameters" do
      assert_log(:trace_params => "CallSid=#{@session.call_id}&Channel=foo&Digits=123&From=999")
      @session[:digits] = '123'
      @session.should_receive(:trace).with("Callback returned application/xml: <Response><Hangup/></Response>")
      @session.should_receive(:push_commands).with([:hangup])

      expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id, :Digits => '123')}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
        CallbackCommand.new(url, :params => {:Digits => :digits}).run @session
      end
    end
  end
end