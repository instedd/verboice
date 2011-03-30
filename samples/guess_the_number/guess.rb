#!/usr/bin/env ruby
require 'rubygems'
require 'sinatra'

Numbers = {}

post '/' do
  session_id = params[:CallSid]
  number = Numbers[session_id]
  pressed = params[:Digits].to_i

  if not number
    Numbers[session_id] = (1..99).to_a.sample
    return guess
  end

  return guess if pressed <= 0

  if number == pressed
    Numbers.delete session_id
    play 'woman.gsm'
  elsif number > pressed
    gather_with_file 'larger.mp3'
  else
    gather_with_file 'smaller.mp3'
  end
end

def guess
  gather_with_file 'guess.mp3'
end

def gather_with_file(file)
  %Q(<Response><Gather numDigits="2"><Play>http://localhost:4567/#{file}</Play></Gather></Response>)
end

def play(file)
  "<Response><Play>http://localhost:4567/#{file}</Play><Hangup /></Response>"
end
