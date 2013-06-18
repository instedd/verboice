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

class NuntiumChannel < ActiveRecord::Base
  belongs_to :account

  attr_accessible :name, :enabled, :kind

  before_destroy :destroy_nuntium_channel
  before_validation :configure_nuntium_channel
  after_save :save_nuntium_channel

  validates_presence_of :account_id
  validates_presence_of :channel_name

  validates_presence_of :name
  validates_uniqueness_of :name, :scope => :account_id

  def kind
    @kind ||= channel.try(:kind)
  end

  def kind=(value)
    unless @kind.present?
      @kind = value
    end
  end

  def channel
    @channel ||= find_or_create_channel
  end

  def save(*)
    begin
      super
    rescue Pigeon::ChannelInvalid
      false
    end
  end

  private

  def find_or_create_channel
    if channel_name.present?
      Pigeon::NuntiumChannel.find(channel_name)
    else
      Pigeon::NuntiumChannel.new kind: @kind 
    end
  end

  def configure_nuntium_channel
    if channel.name.blank?
      channel.name = channel.generate_name
    end
    self.channel_name = channel.name
    true
  end

  def save_nuntium_channel
    # restrict channel to only send messages for the current account
    channel.restrictions = [{ "name" => "account_id", "value" => account_id.to_s }]
    channel.enabled = enabled
    channel.save!
  end

  def destroy_nuntium_channel
    begin
      channel.destroy
    rescue Pigeon::PigeonError => e
      Rails.logger.error "Error destroying associated Nuntium channel: #{e.message}"
    end
  end
end
