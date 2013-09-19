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

class Command
  attr_accessor :next

  include MarshalZipSerializable

  module BrokerFlow
    def self.dump(x)
      return nil unless x
      Zlib.deflate(x.to_a.to_yaml)
    end

    def self.load(x)
      return nil unless x
      YAML.load(Zlib.inflate(x))
    rescue Exception => ex
      Marshal.load(Zlib.inflate(x)) rescue nil
    end
  end

  def self.inherited(subclass)
    subclass.instance_eval { @spec = [] }
  end

  def self.specs
    subclasses.inject({}) do |hash, cmd|
      hash[cmd.name[0 .. -8].underscore] = cmd.spec
      hash
    end
  end

  def self.spec
    @spec
  end

  def last
    n = self
    n = n.next while n.next
    n
  end

  def run(session)
    @next
  end

  def self.param(name, type, options = {})
    @spec << {:name => name, :type => type}.merge(options)
  end

  def ==(other)
    self.compare_to other
  end

  def compare_to(other, visited = Set.new)
    return false unless self.class == other.class
    visited.add self

    (instance_variables | other.instance_variables).each do |var|
      val = instance_variable_get var
      other_val = other.instance_variable_get var
      if val.is_a? Command
        next if visited.include? val
        return false unless val.compare_to(other_val, visited)
      else
        return false if val != other_val
      end
    end

    true
  end

  def serialize_parameters
    nil
  end

  def to_a
    out = []
    queue = [self]
    visited = {}

    until queue.empty?
      current = queue.shift
      until current.nil?
        if visited.include? current
          out << [:goto, visited[current]]
          break
        end

        visited[current] = out.length

        command = current.class.name.split('::').last.sub('Command', '').underscore.to_sym
        parameters = current.serialize_parameters

        if parameters
          parameters.values.select {|x| x.is_a?(Command)}.each do |cmd|
            queue << cmd
          end
          out << [command, parameters]
        else
          out << command
        end

        current = current.next
      end
      out << :stop
    end

    out.each do |item|
      if item.is_a?(Array) && (parameters = item[1]).is_a?(Hash)
        parameters.each do |key, value|
          if value.is_a?(Command)
            parameters[key] = visited[value]
          end
        end
      end
    end

    out
  end

end
