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
  end
end