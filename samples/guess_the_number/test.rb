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
