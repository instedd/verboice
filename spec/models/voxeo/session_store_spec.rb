require 'spec_helper'

describe Voxeo::SessionStore do

  let(:store) { Voxeo::SessionStore.clone.instance }
  let(:session) { double('session') }

  before(:each) do
    Voxeo::Session.stub(:new).and_return(session)
  end

  it 'should create session if not found and return the same on later calls' do
    Voxeo::Session.should_receive(:new).once.and_return(session)
    store.should_receive(:renew_em_timer).with(:key).twice
    store.session_for(:key).should eq(session)
    store.session_for(:key).should eq(session)
  end

  context 'timers' do
    it 'should add timer when getting session' do
      EM.should_receive(:add_timer).with(Voxeo::SessionStore::Timeout)
      EM.should_not_receive(:cancel_timer)

      store.session_for(:key)
    end

    it 'should renew timer when getting session' do
      timer = double('timer')
      EM.should_receive(:add_timer).and_return(timer)

      store.session_for(:key)

      EM.should_receive(:cancel_timer).with(timer)
      EM.should_receive(:add_timer)

      store.session_for(:key)
    end

    it 'should resume fiber with exception and clear session when timeout' do
      timer_block = nil
      fiber = double('fiber')
      session.should_receive(:get_fiber).and_return(fiber)
      fiber.should_receive(:resume).with(Exception.new("Session timeout"))
      EM.should_receive(:add_timer) do |timeout, &block|
        timer_block = block
      end

      store.session_for(:key)

      timer_block.call

      store.instance_eval{@sessions[:key]}.should be_nil
      store.instance_eval{@timers[:key]}.should be_nil
    end

  end

end

describe Voxeo::Session do

  let(:session) { Voxeo::Session.new }

  it 'should be empty' do
    session.get_fiber.should be_nil
    session.get(:key1).should be_nil
    session.get(:key2).should be_nil
  end

  it 'should store fiber' do
    session.store_fiber :some_fiber
    session.get_fiber.should eq(:some_fiber)
  end

  it 'should store key value pairs' do
    session.store(:key1, :value1)
    session.store(:key2, :value2)
    session.get(:key1).should eq(:value1)
    session.get(:key2).should eq(:value2)
  end

  it 'should end session and clear fiber' do
    session.store_fiber :some_fiber
    session.end!
    session.get_fiber.should be_nil
  end

end