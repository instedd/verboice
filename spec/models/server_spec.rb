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

describe Server do
  it "register with '1'" do
    Server.new('host', nil, '1').should be_register
  end

  it "not register with '0'" do
    Server.new('host', nil, '0').should_not be_register
  end

  it "register with true" do
    Server.new('host', nil, true).should be_register
  end

  it "not register with false" do
    Server.new('host', nil, false).should_not be_register
  end
end