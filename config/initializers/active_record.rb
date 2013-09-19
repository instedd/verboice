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

class ActiveRecord::Base
  def self.config_accessor(*names)
    options = names.extract_options!
    default = options[:default]

    names.each do |name|
      self.config_attrs << name

      define_method(name) do
        config.try(:[], name.to_s) || default
      end

      define_method("#{name}=") do |value|
        respond_to?(:encrypted_config_will_change!) ? encrypted_config_will_change! : config_will_change!

        self.config ||= {}
        self.config[name.to_s] = value

        # this is needed so attr_encrypted encrypts the value correctly
        self.config = config
      end
    end
  end

  def self.config_attrs
    # inherit config attributes
    @config_attrs ||= (self.superclass.respond_to?(:config_attrs) ? self.superclass.config_attrs.clone : [])
  end

  def self.broker_cached(atom = self.name.underscore.to_sym)
    after_commit do
      BrokerClient.invalidate_cache atom, id
    end
  end
end
