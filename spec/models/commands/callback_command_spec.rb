# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

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
      @session.call_flow = CallFlow.make mode: :callback_url
      @default_body = {:From => '999', :Channel => 'foo'}
      @session.call_log = CallLog.make
      apply_call_flow
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
      result = expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
        CallbackCommand.new(url).run @session
      end
      result.should == Commands::HangupCommand.new
    end

    it "interpolates url with session variables" do
      options = {:variables => {'foo' => 'var_foo', 'bar' => 'value_bar', 'baz' => '42'}}
      url = 'http://www.domain.com/{foo}?key1={bar}&key2={baz}'
      interpolated_url = 'http://www.domain.com/the_foo?key1=the_bar&key2=the_42'
      @session.should_receive(:eval).with('var_foo').at_least(:once).and_return('the_foo')
      @session.should_receive(:eval).with('value_bar').at_least(:once).and_return('the_bar')
      @session.should_receive(:eval).with('42').at_least(:once).and_return('the_42')

      expect_em_http :post, interpolated_url, :with => {:body => @default_body.merge(:CallSid => @session.call_id, 'foo' => 'the_foo', 'bar' => 'the_bar', 'baz' => 'the_42')}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
        CallbackCommand.new(url, options).run @session
      end
    end

    it "interpolates url with external service global settings" do
      options = {:external_service_guid => 7}
      external_service = double('external_service')
      external_service.should_receive(:global_variable_value_for).with('foo_global').and_return('the_foo_global')
      ExternalService.should_receive(:find_by_guid).with(7).and_return(external_service)

      url = 'http://www.domain.com/{foo_global}'
      interpolated_url = 'http://www.domain.com/the_foo_global'

      expect_em_http :post, interpolated_url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
        CallbackCommand.new(url, options).run @session
      end
    end

    it "runs with hash variable" do
      @session.eval 'bar = {"a": {"b": 1}}'
      options = {:variables => {'foo' => 'bar'}}
      result = expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id, "foo[a][b]" => 1)}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
        CallbackCommand.new(url, options).run @session
      end
      result.should == Commands::HangupCommand.new
    end

    context "running with an app which has http basic authentication for the callback url", :focus => true do
      before do
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
      result = expect_em_http :get, url, :with => @default_body.merge(:CallSid => @session.call_id), :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
        CallbackCommand.new(url, :method => :get).run(@session)
      end
      result.should == Commands::HangupCommand.new
    end

    it "run without url" do
      @session.should_receive(:callback_url).and_return(url)

      result = expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
        CallbackCommand.new.run @session
      end
      result.should == Commands::HangupCommand.new
    end

    it "run receives json in response" do
      result = expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => 'hangup();', :content_type => 'application/json' do
        CallbackCommand.new(url).run @session
      end
      result.should == Commands::JsCommand.new('hangup();')
    end

    it "run with custom parameters" do
      @session[:digits] = '123'
      result = expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id, :Digits => '123')}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
        CallbackCommand.new(url, :params => {:Digits => :digits}).run @session
      end
      result.should == Commands::HangupCommand.new
    end

    it "continues with following commands after callback result" do
      @session.should_receive(:callback_url).and_return(url)

      result = expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Pause/></Response>', :content_type => 'application/xml' do
        Compiler.make do
          Callback()
          Hangup()
        end.run @session
      end
      result.should == Compiler.make { Pause(); Hangup() }
    end

    describe "variables" do
      let(:options)  { {:response_type => :variables}}
      let(:response) { {:key1 => 'value1', :key2 => 'value2'} }

      it "assigns returned variables to session" do
        result = expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => response.to_json, :content_type => 'application/json' do
          CallbackCommand.new(url, options).run @session
        end

        @session['response_key1'].should eq('value1')
        @session['response_key2'].should eq('value2')
      end

      it "should eval returned variables in session" do
        @session.should_receive(:eval).with("'value1'")
        @session.should_receive(:eval).with("'value2'")

        result = expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => response.to_json, :content_type => 'application/json' do
          CallbackCommand.new(url, options).run @session
        end
      end

      it "should escape javascript" do
        response = {:key1 => "foo'bar"}

        @session.should_receive(:eval).with("'foo\\'bar'").and_return("foo'bar")

        result = expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => response.to_json, :content_type => 'application/json' do
          CallbackCommand.new(url, options).run @session
        end

        @session['response_key1'].should eq("foo'bar")
      end

      it "should not quote numbers" do
        response = {:key1 => "7"}

        @session.should_receive(:eval).with('7').and_return(7)

        result = expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => response.to_json, :content_type => 'application/json' do
          CallbackCommand.new(url, options).run @session
        end

        @session['response_key1'].should eq(7)
      end

    end

    describe "none" do
      let(:options) { {:response_type => :none}}

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

    it "should include last entry from call log" do
      @session.call_log.should_receive(:last_entry).and_return(double('last entry', :id => 567))
      result = expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id, :LastEntry => 567)}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
        CallbackCommand.new(url).run @session
      end
    end

  end
end