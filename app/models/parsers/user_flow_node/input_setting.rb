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

module Parsers
  module UserFlowNode
    class InputSetting

      include JavascriptUtils
      attr_accessor :value, :variable, :step, :response

      def initialize(opts)
        options = opts.with_indifferent_access
        self.value = options['value']
        self.variable = options['variable']
        self.step = options['step']
        self.response = options['response']
      end

      def expression()
        if step.present?
          "value_#{step}"
        elsif variable.present?
          "var_#{variable}"
        elsif value.present?
          value_for_js value
        elsif response.present?
          "external_#{response}"
        else
          'null'
        end
      end
    end
  end
end
