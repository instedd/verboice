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

describe BasicObject do

  it "should give all the subclasses of an object" do
    expect(Foo.all_subclasses.size).to eq(4)
    expect(Foo.all_subclasses).to include(Bar)
    expect(Foo.all_subclasses).to include(Zzz)
    expect(Foo.all_subclasses).to include(Zzzz)
    expect(Foo.all_subclasses).to include(Asdf)
  end
  
  it "should give all leaf subclasses of an object" do
    expect(Foo.all_leaf_subclasses.size).to eq(3)
    expect(Foo.all_leaf_subclasses).to include(Zzz)
    expect(Foo.all_leaf_subclasses).to include(Zzzz)
    expect(Foo.all_leaf_subclasses).to include(Asdf)
  end

  it "should give the subclasses of an object" do
    expect(Foo.subclasses.size).to eq(2)
    expect(Foo.subclasses).to include(Bar)
    expect(Foo.subclasses).to include(Asdf)
  end

end

class Foo
end
class Bar < Foo
end
class Zzz < Bar
end
class Zzzz < Bar
end
class Asdf < Foo
end