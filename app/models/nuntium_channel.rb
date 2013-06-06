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

  attr_accessible :name, :enabled

  before_destroy :destroy_nuntium_channel

  validates_presence_of :account_id
  validates_presence_of :channel_name

  validates_presence_of :name
  validates_uniqueness_of :name, :scope => :account_id

  private

  def destroy_nuntium_channel
    begin
      Pigeon::NuntiumChannel.find(channel_name).destroy
    rescue Pigeon::PigeonError => e
      Rails.logger.error "Error destroying associated Nuntium channel: #{e.message}"
    end
  end
end
