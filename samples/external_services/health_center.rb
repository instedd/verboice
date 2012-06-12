#!/usr/bin/env ruby
require 'rubygems'
require 'sinatra'

Results = {0 => 'positive', 1 => 'negative', 2 => 'not_available'}
AvailableDates = ["Tuesday 7 AM", "Wednesday 9 AM", "Friday 2 PM"]
Appointments = {}

get '/manifest' do
  content_type 'application/xml'
  File.read('manifest.xml')
end

post '/results' do
  content_type 'application/json'
  pin = params[:pin].to_i
  result = pin % 3
  "{\"result\": \"#{Results[result]}\"}"
end

post '/available_dates' do
  content_type 'application/xml'
  dates = []
  AvailableDates.each_with_index do |date, i|
    dates << "Option #{i+1} for #{date}."
  end
  "<Response><Say>#{dates.join(' ')}</Say></Response>"
end

post '/make_appointment' do
  pin = params[:pin]
  date_option = params[:date_option].to_i - 1
  Appointments[pin] = AvailableDates[date_option]
  nil
end

post '/appointments' do
  content_type 'application/xml'
  pin = params[:pin]
  appointment = Appointments[pin]
  if appointment
    response = "You have an appointment for #{appointment}"
  else
    response = "You don't have any booked appointments"
  end
  "<Response><Say>#{response}</Say></Response>"
end
