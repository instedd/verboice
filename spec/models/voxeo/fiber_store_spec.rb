require 'spec_helper'

describe Voxeo::FiberStore do
  let(:store) { Voxeo::FiberStore.clone.instance }
  
  it 'stores fibers' do
    store.store_fiber(:key, :fiber).should eq(:fiber)
    store.get_fiber_for(:key).should eq(:fiber)
  end
  
  it 'should return nil if fiber not found' do
    store.get_fiber_for(:key).should be_nil
  end
  
  it 'should detele fiber' do
    store.store_fiber(:key, :fiber)
    store.get_fiber_for(:key).should_not be_nil
    store.delete_fiber_for(:key).should eq(:fiber)
    store.get_fiber_for(:key).should be_nil
  end
  
  context 'timers' do
    
    it 'should add timer when storing fiber' do
      EM.should_receive(:add_timer).with(Voxeo::FiberStore::TIMEOUT)
      EM.should_not_receive(:cancel_timer)
      
      store.store_fiber(:key, :fiber)
    end
    
    it 'should renew timer when asking for fiber' do
      timer = double('timer')
      EM.should_receive(:add_timer).and_return(timer)
      EM.should_receive(:add_timer)
      EM.should_receive(:cancel_timer).with(timer)
      
      store.store_fiber(:key, :fiber)
      store.get_fiber_for(:key)
    end
    
    it 'should not renew timer when asking for an invalid key' do
      EM.should_not_receive(:add_timer)
      EM.should_not_receive(:cancel_timer)
      
      store.get_fiber_for(:invalid_key)
    end
    
    it 'should cancel timer when deleting fiber' do
      EM.should_receive(:add_timer).and_return(:timer)
      store.store_fiber(:key, :fiber)
      
      EM.should_receive(:cancel_timer).with(:timer)
      store.delete_fiber_for(:key)
    end
    
    it 'should resume fiber with exception when timeout' do
      fiber = double('fiber')
      fiber.should_receive(:resume).with(Exception.new("Session timeout"))
      EM.should_receive(:add_timer) do |timeout, &block|
        block.call
      end
    
      store.store_fiber(:key, fiber)
      store.get_fiber_for(:key).should be_nil
    end
        
  end
  
end