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

describe File do
  it "get mime type of mp3 file" do
    mp3_path = File.expand_path '../../fixtures/sample.mp3', __FILE__
    File.mime_type(mp3_path).should == 'audio/mpeg'
  end

  it "get mime type of wav file" do
    wav_path = File.expand_path '../../fixtures/sample.wav', __FILE__
    File.mime_type(wav_path).should == 'audio/x-wav'
  end

  it "detect mp3 file" do
    mp3_path = File.expand_path '../../fixtures/sample.mp3', __FILE__
    File.is_mpeg?(mp3_path).should == true
  end

  it "detect wav file" do
    wav_path = File.expand_path '../../fixtures/sample.wav', __FILE__
    File.is_wav?(wav_path).should == true
  end

end
