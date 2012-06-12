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
  confirmed_at { 2.days.ago }
end

Project.blueprint do
  account
  name
end

CallFlow.blueprint do
  project
  name
end

Trace.blueprint do
  call_flow
  step_id {1}
  call_log
  result
end

CallLog.blueprint do
  channel
  call_flow { channel.call_flow }
  project { call_flow.project }
  account { project.account }
end

Channel.blueprint do
  call_flow
  account { call_flow.project.account }
  name
end

Channel.blueprint(:voxeo) do
  kind { "voxeo" }
  token { Sham.guid }
  url
end

Schedule.blueprint do
  project
  name
end

QueuedCall.blueprint do
  channel
  call_log
  address { Sham.password }
end

PersistedVariable.blueprint do
  contact
  name
  value { rand(9999) }
end

RecordedAudio.blueprint do
  call_log
  contact
  description { Faker::Name.name }
  key { Sham.guid }
end

Contact.blueprint do
  project
  address { Sham.password }
end

ExternalService.blueprint do
  name
  url
  project
end

ExternalServiceStep.blueprint do
  external_service
  name
  display_name { name }
  icon { Sham.url }
  callback_url { Sham.url }
end

CallLogEntry.blueprint do
end
