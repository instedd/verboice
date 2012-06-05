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

describe SuitableClassFinder do

  it "should find a direct subclass suitable for an object" do
    (SuitableClassFinder.find_direct_subclass_of AbstractClass1, suitable_for: 1).should eq(ConcreteClass1)
    (SuitableClassFinder.find_direct_subclass_of AbstractClass1, suitable_for: 3).should eq(AbstractClass2)
  end
  it "should find a class suitable for an object from a given collection of classes" do
    (SuitableClassFinder.find_in [ConcreteClass1, ConcreteClass2], suitable_for: 1).should eq(ConcreteClass1)
  end

  it "should find a leaf subclass of a class for a given object" do
    (SuitableClassFinder.find_leaf_subclass_of AbstractClass1, suitable_for: 2).should eq(ConcreteClass2)
  end

  it "should be able to define a custom message to send to the classes to search" do
    (SuitableClassFinder.find_in [ConcreteClass1, ConcreteClass2], suitable_for: 2, sending: :foo?).should eq(ConcreteClass2)
  end

  it "shuld be able to define a behavior when no class is found" do
    (SuitableClassFinder.find_leaf_subclass_of AbstractClass1, suitable_for: 1, if_none: lambda{|finder| 'foo'} ).should eq(ConcreteClass1)
    (SuitableClassFinder.find_leaf_subclass_of AbstractClass1, suitable_for: 12345, if_none: lambda{|finder| 'foo'}).should eq('foo')
  end

  it "should be able to define a behavior when multiple classes are found"do
    (SuitableClassFinder.find_leaf_subclass_of AbstractClass1,
      suitable_for: 1,
      if_multiple: lambda{|potential_classes, finder|
        'foo'
      }
    ).should eq(ConcreteClass1)
    (SuitableClassFinder.find_leaf_subclass_of AbstractClass1,
      suitable_for: 12345,
      sending: :bar?,
      if_multiple: lambda{|potential_classes, finder|
        'foo'
      }
    ).should eq('foo')
  end

  it "should be able to add extra collaborators if the condition requires it" do
    (SuitableClassFinder.find_leaf_subclass_of AbstractClass1, suitable_for: [1, 2, 3], sending: :zzz?).should eq(ConcreteClass1)
    (SuitableClassFinder.find_leaf_subclass_of AbstractClass1, suitable_for: [3, 2, 3], sending: :zzz?).should eq(ConcreteClass3)
    (SuitableClassFinder.find_leaf_subclass_of AbstractClass1, suitable_for: [3, {a:2, b:3}], sending: :nn?).should eq(ConcreteClass2)
    (SuitableClassFinder.find_leaf_subclass_of AbstractClass1, suitable_for: [3, {c:2, d:3}], sending: :nn?).should eq(ConcreteClass3)
  end

  it "should allow to specify a block of actions to be carried with the class that has been found" do
    (SuitableClassFinder.find_leaf_subclass_of AbstractClass1,
      suitable_for: 1,
      if_found: lambda{ |class_found|
        'foo'
      }
    ).should eq('foo')
    (SuitableClassFinder.find_leaf_subclass_of AbstractClass1,
      suitable_for: 1,
      if_found: lambda{|class_found|
        class_found
      }
    ).should eq(ConcreteClass1)
    (SuitableClassFinder.find_leaf_subclass_of AbstractClass1,
      suitable_for: 12345,
      sending: :bar?,
      if_multiple: lambda{|potential_classes, finder|
        'foo'
      },
      if_found: lambda{|class_found|
        'bar'
      }
    ).should eq('foo')
  end

end

class AbstractClass1
  def self.can_handle? a_number
    false
  end
  def self.bar? a_number
    true
  end
  def self.zzz? first_number, second_number, third_number
    false
  end
  def self.nn? a_number, options
    false
  end
end

class ConcreteClass1 < AbstractClass1
  def self.can_handle? a_number
    a_number == 1 || a_number == 0
  end
  def self.foo? a_number
    a_number == 32
  end
  def self.zzz? first_number, second_number, third_number
    first_number == 1 && second_number == 2 && third_number == 3
  end
end

class AbstractClass2 < AbstractClass1
  def self.can_handle? a_number
    a_number == 2 || a_number == 0 || a_number == 3
  end
end

class ConcreteClass2 < AbstractClass2
  def self.can_handle? a_number
      a_number == 2 || a_number == 0
  end
  def self.foo? a_number
    a_number == 2
  end
  def self.nn? a_number, options
    a_number == 3 && options[:a] == 2 && options[:b] == 3
  end
end

class ConcreteClass3 < AbstractClass2
  def self.can_handle? a_number
    a_number == 3 || a_number == 0
  end
  def self.zzz? first_number, second_number, third_number
    first_number == 3 && second_number == 2 && third_number == 3
  end
  def self.nn? a_number, options
    a_number == 3 && options[:c] == 2 && options[:d] == 3
  end
end
