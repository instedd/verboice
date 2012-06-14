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

# Expects that a given ActiveRecord model will have the given records.
#
# Examples
# --------
#
# # Expect two people in the database with the given attributes
# Person.should have_records [{name: 'David', age: 12}, {name: 'Peter', age: 13}]
#
# # Expect one person in the database with the given attributes
# Person.should have_records name: 'David', age: 12
#
# # Expect two people in the database, both with age 13 and then
# # the first named 'David' and the second 'Peter'. The first hash acts as common
# # attributes to check on all records
# Person.should have_records {age: 13}, [{name: 'David'}, {name: 'Peter'}]
#
# --------
#
# To expect date or datetimes you can supply as String and it will be parsed
# with Time.parse and be converted to a Date with to_date if the column is a date.
#
# Person.should have_records created_at: '10-09-2011 10:00:00'
#
# To skip one of the models just use an empty hash:
#
# # Expect two people, don't care about the first one and the second one's name should be David
# Person.should have_records [{}, {name: 'David'}]
RSpec::Matchers.define :have_records do |*expected|
  match do |actual|
    if expected.length == 1
      if expected[0].is_a? Array
        expected = [{}, expected[0]]
      elsif expected[0].is_a? Hash
        expected = [{}, [expected[0]]]
      end
    end

    general, specifics = expected

    models = actual.all
    if models.length != specifics.length
      @error_message = "Expected #{actual.name}.count to be #{specifics.length} but was #{models.length}"
    end

    unless @error_message
      models.each_with_index do |model, i|
        break if @error_message

        check_attributes actual, model, general, i
        check_attributes actual, model, specifics[i], i unless specifics[i].nil? || specifics[i] == '_' || specifics[i] == :_
      end
    end

    !@error_message
  end

  failure_message_for_should do |model|
    @error_message
  end

  def check_attributes(actual, model, attributes, i)
    attributes.each do |key, value|
      break if @error_message

      model_value = model.send key
      if model_value != coerce(actual, key, value)
        @error_message = "Expected #{actual.name} ##{i + 1} #{key} to be '#{value}' but was '#{model_value}'"
      end
    end
  end

  def coerce(model, column_name, value)
    if value.is_a? String
      column_name = column_name.to_s
      if column_name.end_with? '_at'
        column_type = model.columns_hash[column_name].type
        case column_type
        when :date
          return Time.parse(value).to_date
        when :datetime, :time
          return Time.parse(value)
        end
      end
    end

    value
  end
end