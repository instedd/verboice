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

describe HttpBroker::SessionStore do

  let(:store) { HttpBroker::SessionStore.clone.instance }
  let(:session) { double('session') }

  before(:each) do
    HttpBroker::Session.stub(:new).and_return(session)
  end

  it 'should create session if not found and return the same on later calls' do
    HttpBroker::Session.should_receive(:new).once.and_return(session)
    store.should_receive(:renew_em_timer).with(:key).twice
    store.session_for(:key).should eq(session)
    store.session_for(:key).should eq(session)
  end

  context 'timers' do
    it 'should add timer when getting session' do
      EM.should_receive(:add_timer).with(HttpBroker::SessionStore::Timeout)
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

describe HttpBroker::Session do

  let(:session) { HttpBroker::Session.new }

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