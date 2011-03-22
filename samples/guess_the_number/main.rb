require 'rubygems'
require 'sinatra'

numbers = {}

post '/' do
  session_id = params[:CallSid]
  number = numbers[session_id]
  pressed = params[:Digits].to_i

  if number && pressed > 0
    if number == pressed
      '<Response><Play>http://localhost:4567/woman.gsm</Play><Hangup /></Response>'
    elsif number > pressed
      '<Response><Gather numDigits="2"><Play>http://localhost:4567/larger.mp3</Play></Gather></Response>'
    else
      '<Response><Gather numDigits="2"><Play>http://localhost:4567/smaller.mp3</Play></Gather></Response>'
    end
  else
    numbers[session_id] = (1..99).to_a.sample
    '<Response><Gather numDigits="2"><Play>http://localhost:4567/guess.mp3</Play></Gather></Response>'
  end
end
