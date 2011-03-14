require 'machinist/active_record'
require 'sham'
require 'ffaker'

Sham.name { Faker::Name.name }

Application.blueprint do
  name
end
