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

#!/usr/bin/env ruby
require 'guess'
require 'test/unit'
require 'rack/test'

ENV['RACK_ENV'] = 'test'

class GuessTest < Test::Unit::TestCase
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def setup
    Numbers.clear
  end

  def test_first_time
    assert_nil Numbers['1']

    post '/', :CallSid => '1'
    assert last_response.ok?
    assert_match /<Response.*>.*<Gather.*>.*<Play.*>.*guess\.mp3/, last_response.body
    assert 1 <= Numbers['1'] && Numbers['1'] < 100
  end

  def test_nothing_pressed
    Numbers['1'] = 50

    post '/', :CallSid => '1'
    assert last_response.ok?
    assert_match /<Response.*>.*<Gather.*>.*<Play.*>.*guess\.mp3/, last_response.body
    assert_equal 50, Numbers['1']
  end

  def test_larger
    Numbers['1'] = 50
    post '/', :CallSid => '1', :Digits => '49'
    assert last_response.ok?
    assert_match /<Response.*>.*<Play.*>.*larger\.mp3/, last_response.body
    assert_equal 50, Numbers['1']
  end

  def test_smaller
    Numbers['1'] = 50
    post '/', :CallSid => '1', :Digits => '51'
    assert last_response.ok?
    assert_match /<Response.*>.*<Play.*>.*smaller\.mp3/, last_response.body
    assert_equal 50, Numbers['1']
  end

  def test_win
    Numbers['1'] = 50
    post '/', :CallSid => '1', :Digits => '50'
    assert last_response.ok?
    assert_match /<Response.*>.*<Play.*>.*woman\.gsm.*<Hangup.*\/>/, last_response.body
    assert_nil Numbers['1']
  end
end
