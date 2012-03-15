#!/usr/bin/env ruby
require 'rubygems'
require 'sinatra'

Users = {}

post '/' do
  session_id = params[:CallSid]

   "<Response><Dial callerId='+855977100860' channel='trysip2sip'>dwilkie@sip2sip.info</Dial></Response>"

#  if not number
#    Numbers[session_id] = (1..99).to_a.sample
#    return guess request
#  end

#  return guess request if pressed <= 0

#  if number == pressed
#    Numbers.delete session_id
#    play 'woman.gsm'
#  elsif number > pressed
#    gather_with_file 'larger.mp3', request
#  else
#    gather_with_file 'smaller.mp3', request
#  end
end

def guess(request)
  gather_with_file 'guess.mp3', request
end

def gather_with_file(file, request)
  %Q(<Response><Gather numDigits="1"><Play>http://localhost:4567/#{file}</Play></Gather><Redirect>#{request.env['REQUEST_URI']}</Redirect></Response>)
end

def play(file)
  "<Response><Play>http://localhost:4567/#{file}</Play><Hangup /></Response>"
end
