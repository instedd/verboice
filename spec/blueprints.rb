require 'machinist/active_record'
require 'sham'
require 'ffaker'

Sham.define do
  name { Faker::Name.name }
  email { Faker::Internet.email }
  username { Faker::Internet.user_name }
  password { Faker::Name.name[0..10] }
  guid { Guid.new.to_s }
  url { "http://" + Faker::Internet.domain_name }
  result { Faker::Lorem.sentence}
end

Account.blueprint do
  email
  password
end

Application.blueprint do
  account
  name
end

Trace.blueprint do
  application
  step_id {1}
  call_log
  result
end

CallLog.blueprint do
  channel
  application { channel.application }
end

Channel.blueprint do
  application
  account { application.account }
  name
end

Channel.blueprint(:voxeo) do
  kind { "voxeo" }
  token { Sham.guid }
  url
end

CallQueue.blueprint do
  account
  name
end

QueuedCall.blueprint do
  channel
  call_log
  address { Sham.password }
end

PersistedVariable.blueprint do
  account
  address { Sham.password }
  name
  value { rand(9999) }
end


