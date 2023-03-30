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

require 'spec_helper'

describe Channels::Twilio do
  describe "base_url" do
    it "accepts nil" do
      channel = Channels::Twilio.make_unsaved(base_url: nil)
      expect(channel.valid?).to eq(true)
      expect(channel.base_url).to eq(nil)
    end

    it "accepts url" do
      channel = Channels::Twilio.make_unsaved(base_url: "http://domain:8000/resource")
      expect(channel.valid?).to eq(true)
      expect(channel.base_url).to eq("http://domain:8000/resource")
    end

    it "saves blank string as nil" do
      channel = Channels::Twilio.make_unsaved(base_url: "  \t  ")
      expect(channel.valid?).to eq(true)
      expect(channel.base_url).to eq(nil)
    end

    it "strips" do
      channel = Channels::Twilio.make_unsaved(base_url: "   https://domain:8000/resource\t")
      expect(channel.valid?).to eq(true)
      expect(channel.base_url).to eq("https://domain:8000/resource")
    end

    it "chomps trailing forward slash" do
      channel = Channels::Twilio.make_unsaved(base_url: "   https://domain:8000/resource/\t")
      expect(channel.valid?).to eq(true)
      expect(channel.base_url).to eq("https://domain:8000/resource")
    end

    it "refuses non URL" do
      channel = Channels::Twilio.make_unsaved(base_url: "ftp://domain:8000/resource")
      expect(channel.valid?).to eq(false)
      expect(channel.errors[:base_url]).to eq(["is invalid"])
    end
  end
end
