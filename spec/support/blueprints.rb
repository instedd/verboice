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
require 'machinist/object'
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
  number8 { (1..8).map { ('1'..'9').to_a.sample }.join }
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
  channel { Channel.all_leaf_subclasses.sample.make }
  call_flow { channel.call_flow }
  project { call_flow.project }
  account { project.try(:account) || channel.try(:account) || account }
end

Channels::Custom.blueprint do
  call_flow
  account { call_flow.project.account }
  name
end

Channels::CustomSip.blueprint do
  call_flow
  account { call_flow.project.account }
  name
  domain { Sham.url }
  direction { 'both' }
  register { true }
end

Channels::Voxeo.blueprint do
  call_flow
  account { call_flow.project.account }
  name
end

Channels::Twilio.blueprint do
  call_flow
  account { call_flow.project.account }
  name
end

Channels::TemplateBasedSip.blueprint do
  call_flow
  account { call_flow.project.account }
  name
  domain { Sham.url }
  kind { name }
end

Schedule.blueprint do
  project
  name
  time_from { Time.gm(2000, 1, 1, 10, 0) }
  time_to { Time.gm(2000, 1, 1, 11, 0) }
end

QueuedCall.blueprint do
  channel { Channel.all_leaf_subclasses.sample.make }
  call_log
  address { Sham.password }
end

PersistedVariable.blueprint do
  contact
  project_variable
  value { rand(9999) }
end

ProjectVariable.blueprint do
  name
  project
end

RecordedAudio.blueprint do
  call_log
  contact
  description { Faker::Name.name }
  key { Sham.guid }
end

Contact.blueprint do
  project
  addresses { [ContactAddress.make(contact: object)] }
end

ContactAddress.blueprint do
  address { Sham.number8 }
end

ExternalService.blueprint do
  name
  url
  project
  guid
end

ExternalServiceStep.blueprint do
  guid
  external_service
  name
  display_name { name }
  icon { Sham.url }
  kind { "callback" }
  callback_url { Sham.url }
end

CallLogEntry.blueprint do
end

OAuthToken.blueprint do
  account
  access_token { Faker::Name.name }
  refresh_token { Faker::Name.name }
  service { :google }
  expires_at { DateTime.now.utc + 3600.seconds }
end

Resource.blueprint do
  name
  project
  guid
end

UploadLocalizedResource.blueprint do
  resource
  language { 'en' }
  guid
  uploaded_audio { Guid.new.to_s }
  filename { Faker::Name.name }
end

TextLocalizedResource.blueprint do
  resource
  language { 'en' }
  guid
end

RecordLocalizedResource.blueprint do
  resource
  language { 'en' }
  guid
  recorded_audio { Guid.new.to_s }
end

UrlLocalizedResource.blueprint do
  resource
  language { 'en' }
  guid
  url
end

CallFlowExternalService.blueprint do
  call_flow
  external_service
end

