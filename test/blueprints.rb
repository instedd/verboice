require 'machinist/active_record'
require 'sham'
require 'ffaker'

Sham.define do
  name { Faker::Name.name }
  email { Faker::Internet.email }
  username { Faker::Internet.user_name }
  password { Faker::Name.name }
end

Account.blueprint do
  email
  password
end

Application.blueprint do
  account
  name
end

CallLog.blueprint do
  application
  account { application.account }
  details { '' }
end
