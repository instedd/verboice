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
require 'rubygems'
require 'sinatra'

Numbers = {}

post '/' do
  p params
  session_id = params[:CallSid]
  number = Numbers[session_id]
  pressed = params[:Digits].to_i

  if not number
    Numbers[session_id] = (1..99).to_a.sample
    return guess request
  end

  return guess request if pressed <= 0

  if number == pressed
    Numbers.delete session_id
    play 'woman.gsm'
  elsif number > pressed
    gather_with_file 'larger.mp3', request
  else
    gather_with_file 'smaller.mp3', request
  end
end

def guess(request)
  gather_with_file 'guess.mp3', request
end

def gather_with_file(file, request)
  %Q(<Response><Gather numDigits="2"><Play>http://localhost:4567/#{file}</Play></Gather><Redirect>#{request.env['REQUEST_URI']}</Redirect></Response>)
end

def play(file)
  "<Response><Play>http://localhost:4567/#{file}</Play><Hangup /></Response>"
end
