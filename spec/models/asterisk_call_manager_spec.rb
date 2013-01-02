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

describe Asterisk::CallManager do
  before(:each) do
    @call_manager = Asterisk::CallManager.new 1
  end

  [
    [:session_id, 'arg_1'],
    [:caller_id, 'callerid']
  ].each do |method, key|
    it "#{method}" do
      @call_manager.should_receive(:[]).with(key).and_return :id
      @call_manager.send(method).should == :id
    end
  end

  it "channel_id" do
    @call_manager.env['extension'] = '123'
    @call_manager.channel_id.should == 123
  end

  it "channel_id from argument" do
    @call_manager.env['arg_2'] = '123'
    @call_manager.channel_id.should == 123
  end

  it 'answer' do
    @call_manager.should_receive(:send_command).with('ANSWER')
    @call_manager.answer
  end

  it 'hangup' do
    @call_manager.should_receive(:send_command).with('HANGUP')
    @call_manager.should_receive :close_connection
    @call_manager.hangup
  end

  it 'bridge_with' do
    @call_manager.should_receive(:send_command).with(*%w(EXEC Bridge MY/channel))
    other_call_manager = Asterisk::CallManager.new 2
    other_call_manager.env['channel'] = 'MY/channel'
    @call_manager.bridge_with Session.new(pbx: other_call_manager)
  end

  it 'pauses' do
    EM.should_receive(:fiber_sleep).with(13)
    @call_manager.pause(13)
  end

  it 'say' do
    session = stub('session')
    session.should_receive(:synth).with('some text').and_return(:filename)

    @call_manager.session = session
    @call_manager.should_receive(:play).with(:filename, {})

    @call_manager.say 'some text'
  end

  context "play" do
    before(:each) do
      @path = @call_manager.sound_path_for 'something'
    end

    it "play" do
      @call_manager.should_receive(:stream_file).with('verboice/something', nil).and_return(line '1'.ord.to_s)
      value = @call_manager.play @path
      value.should == ['1', nil]
    end

    it "play with escape digits" do
      @call_manager.should_receive(:stream_file).with('verboice/something', '123').and_return(line '0')
      value = @call_manager.play @path, {}, '123'
      value.should == [nil, nil]
    end

    it "play throws exception when fails" do
      @call_manager.should_receive(:stream_file).and_return(Batphone::AgiMixin::Response.new "200 result=0 endpos=0")
      assert_raise(Exception, 'Error while playing file') { @call_manager.play 'foo' }
    end

    it "play throws exception when user hangs up" do
      @call_manager.should_receive(:stream_file).and_return(line '-1')
      assert_raise(Exception, 'User hung up') { @call_manager.play 'foo' }
    end
  end

  context "capture" do
    {'48' => '0',
     '49' => '1',
     '57' => '9',
     '35' => '#',
     '42' => '*',
     '0' => :timeout}.each do |result, digit|
      it "capture one digit #{digit}" do
        expect_digit result
        value = @call_manager.capture :min => 1, :max => 1, :finish_on_key => '', :timeout => 5
        value.should == digit
      end
     end

    it "capture two digits" do
      expect_digits '42'
      value = @call_manager.capture :min => 2, :max => 2, :finish_on_key => '#', :timeout => 5
      value.should == '42'
    end

    it "capture three digits timeout" do
      expect_digits ['4'.ord.to_s, '2'.ord.to_s, '0']
      value = @call_manager.capture :min => 3, :max => 3, :finish_on_key => '#', :timeout => 5
      value.should == :timeout
    end

    it "capture digits finishes on key" do
      expect_digits '42#'
      value = @call_manager.capture :min => 1, :max => 5, :finish_on_key => '#', :timeout => 5
      value.should == '42'
    end

    it "capture digits and press finish key the first time" do
      expect_digits '#'
      value = @call_manager.capture :min => 1, :max => 5, :finish_on_key => '#', :timeout => 5
      value.should == :finish_key
    end

    it "capture digits and timeout" do
      expect_digit '0'
      value = @call_manager.capture :min => 1, :max => 5, :finish_on_key => '#', :timeout => 5
      value.should == :timeout
    end

    it "capture digits while playing" do
      @call_manager.should_receive(:play).ordered.with('some_file', anything, '0123456789#*').and_return('4')
      expect_digit '2'.ord.to_s
      value = @call_manager.capture :min => 2, :max => 2, :finish_on_key => '#', :timeout => 5, :play => 'some_file'
      value.should == '42'
    end

    it "capture digits with string values" do
      @call_manager.should_receive(:play).ordered.with('some_file', anything, '0123456789#*').and_return('4')
      expect_digit '2'.ord.to_s
      value = @call_manager.capture :min => '2', :max => '2', :finish_on_key => '#', :timeout => '5', :play => 'some_file'
      value.should == '42'
    end

    it "capture digits and play is finish key" do
      @call_manager.should_receive(:play).ordered.with('some_file', anything, '0123456789#*').and_return('*')
      value = @call_manager.capture :min => 2, :max => 2, :finish_on_key => '*', :timeout => 5, :play => 'some_file'
      value.should == :finish_key
    end

    it "capture digits and play nothing pressed" do
      @call_manager.should_receive(:play).ordered.with('some_file', anything, '0123456789#*').and_return(nil)
      expect_digits '24'
      value = @call_manager.capture :min => 2, :max => 2, :finish_on_key => '*', :timeout => 5, :play => 'some_file'
      value.should == '24'
    end

    it "capture digits and play just one digit" do
      @call_manager.should_receive(:play).ordered.with('some_file', anything, '0123456789#*').and_return('1')
      value = @call_manager.capture :min => 1, :max => 1, :finish_on_key => '*', :timeout => 5, :play => 'some_file'
      value.should == '1'
    end

    def expect_digits(digits)
      if digits.is_an? Array
        digits.each { |digit| expect_digit digit }
      else
        digits.each_char { |c| expect_digit c.ord.to_s }
      end
    end

    def expect_digit(result)
      @call_manager.should_receive(:wait_for_digit).ordered.with(5 * 1000).and_return(line result)
    end
  end

  context "dial" do
    it "dial number and return successfully" do
      @call_manager.should_receive(:exec).ordered.with('Dial', '1234,60,m')
      @call_manager.should_receive(:get_variable).ordered.with('DIALSTATUS').and_return(asterisk_response('ANSWER'))
      value = @call_manager.dial '1234'
      value.should == :completed
    end

    it "dial number and return busy" do
      @call_manager.should_receive(:exec).ordered.with('Dial', '1234,60,m')
      @call_manager.should_receive(:get_variable).ordered.with('DIALSTATUS').and_return(asterisk_response('BUSY'))
      value = @call_manager.dial '1234'
      value.should == :busy
    end

    it "dial number and return no answer" do
      @call_manager.should_receive(:exec).ordered.with('Dial', '1234,60,m')
      @call_manager.should_receive(:get_variable).ordered.with('DIALSTATUS').and_return(asterisk_response('NOANSWER'))
      value = @call_manager.dial '1234'
      value.should == :no_answer
    end

    it "dial number and return failed" do
      @call_manager.should_receive(:exec).ordered.with('Dial', '1234,60,m')
      @call_manager.should_receive(:get_variable).ordered.with('DIALSTATUS').and_return(asterisk_response('XXXX'))
      value = @call_manager.dial '1234'
      value.should == :failed
    end

    it "raise exception when user hangs up" do
      @call_manager.should_receive(:exec).ordered.with('Dial', '1234,60,m')
      @call_manager.should_receive(:get_variable).ordered.with('DIALSTATUS').and_return(asterisk_response('CANCEL'))
      assert_raise(Exception) { @call_manager.dial '1234' }
    end

    it "dial with custom caller id" do
      @call_manager.should_receive(:set_callerid).ordered.with('"foo" <1234>')
      @call_manager.should_receive(:exec).ordered.with('Dial', '1234,60,m')
      @call_manager.should_receive(:get_variable).ordered.with('DIALSTATUS').and_return(asterisk_response('ANSWER'))
      @call_manager.dial '1234', :caller_id => '"foo" <1234>'
    end
  end

  it 'should record' do
    File.should_receive(:exists?).with('/path/to/filename.wav').and_return(false)
    FileUtils.should_receive(:touch).with('/path/to/filename.wav')
    @call_manager.should_receive(:record_file).with('/path/to/filename', 'wav', '26#', '7000', 'beep')
    File.should_receive(:chmod).with(0666, '/path/to/filename.wav').ordered
    File.should_receive(:chmod).with(0644, '/path/to/filename.wav').ordered
    @call_manager.record '/path/to/filename.wav', '26#', 7
  end

  def asterisk_response(note)
    Batphone::AgiMixin::Response.new("200 result=1 (#{note})")
  end

  def line(result)
    Batphone::AgiMixin::Response.new "200 result=#{result}"
  end
end
