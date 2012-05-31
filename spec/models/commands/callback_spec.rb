require 'spec_helper'

module Commands
  describe Callback do
    let(:dummy) do
      Class.new do
        extend(Commands::Callback)
      end
    end

    let(:url) { 'http://www.foo.bar' }
    let(:auth) { {:user => 'username', :password => 'password'} }
    let(:fiber) { double('fiber') }
    let(:request) { double('request') }
    let(:http) { double('http') }
    let(:params) { {:key1 => 'value1', :key2 => 'value2'} }

    before(:each) do
      EventMachine::HttpRequest.should_receive(:new).with(url).and_return(request)
    end

    context 'method, auth and params' do
      before(:each) do
        http.stub(:callback)
        http.stub(:errback)
        Fiber.stub(:current).and_return(:fiber)
        Fiber.stub(:yield)
      end

      it 'defaults to post' do
        request.should_receive(:post).and_return(http)
        dummy.callback url
      end

      it 'authenticates' do
        request.should_receive(:post).with({:head => {'authorization' => ['username', 'password']}}).and_return(http)
        dummy.callback url, {:authentication => auth}
      end

      it 'gets' do
        request.should_receive(:get).and_return(http)
        dummy.callback url, {:method => :get}
      end

      it 'gets with query string' do
        request.should_receive(:get).with({:query => params}).and_return(http)
        dummy.callback url, {:params => params, :method => 'get'}
      end

      it 'posts with body' do
        request.should_receive(:post).with({:body => params}).and_return(http)
        dummy.callback url, {:params => params, :method => 'POST'}
      end
    end

    context 'callback' do
      let(:response_header) { double('response_header') }

      before(:each) do
        http.stub(:errback)
        http.stub(:response_header).and_return(response_header)

        Fiber.should_receive(:current).and_return(fiber)

        request.should_receive(:post).and_return(http)
      end

      it 'resumes fiber with http when ok' do
        callback_block = nil
        http.should_receive(:callback) do |&block|
          callback_block = block
        end

        Fiber.should_receive(:yield).and_return(http)

        dummy.callback(url).should eq(http)

        # Inside block
        response_header.should_receive(:status).and_return('200')
        fiber.should_receive(:resume).with(http)

        callback_block.call
      end

      it 'resumes fiber with exception when bad' do
        exception = double('exception')

        callback_block = nil
        http.should_receive(:callback) do |&block|
          callback_block = block
        end

        Fiber.should_receive(:yield).and_return(exception)

        dummy.callback(url).should eq(exception)

        # Inside block
        response_header.should_receive(:status).twice.and_return('400')
        fiber.should_receive(:resume).with do |exception|
          exception.should be_a_kind_of(Exception)
          exception.message.should eq('Callback failed with status 400')
        end

        callback_block.call
      end
    end

    context 'errback' do
      before(:each) do
        http.stub(:callback)
        Fiber.should_receive(:current).and_return(fiber)
        request.should_receive(:post).and_return(http)
      end

      it 'resumes fiber with http error exception' do
        exception = double('exception')

        errback_block = nil
        http.should_receive(:errback) do |&block|
          errback_block = block
        end

        Fiber.should_receive(:yield).and_return(exception)

        dummy.callback(url).should eq(exception)

        # Inside block
        http.should_receive(:error).twice.and_return('Http Error Message')
        fiber.should_receive(:resume).with do |exception|
          exception.should be_a_kind_of(Exception)
          exception.message.should eq('Http Error Message')
        end

        errback_block.call
      end

      it 'resumes fiber with default error exception' do
        exception = double('exception')

        errback_block = nil
        http.should_receive(:errback) do |&block|
          errback_block = block
        end

        Fiber.should_receive(:yield).and_return(exception)

        dummy.callback(url).should eq(exception)

        # Inside block
        http.should_receive(:error).and_return(nil)
        fiber.should_receive(:resume).with do |exception|
          exception.should be_a_kind_of(Exception)
          exception.message.should eq("Failed to communicate with #{url}")
        end

        errback_block.call
      end
    end

  end
end