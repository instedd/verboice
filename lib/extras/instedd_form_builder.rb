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

class InsteddFormBuilder < ActionView::Helpers::FormBuilder

  delegate :content_tag, :concat, :capture, :to => :@template

  def initialize(object_name, object, template, options, proc)
    super
  end

  def self.create_labeled_field(method_name)
    define_method(method_name) do |name, *args|
      options = args.clone.extract_options!
      content_tag(:div, label(name, options.delete(:label), options.delete(:label_options) || {}) + super(name, *args), :class => 'field')
    end
  end

  def self.helpers_not_included_by_default
    ['collection_select', 'enum_select', 'select', 'time_zone_select']
  end

  def self.helpers_that_mustnt_be_redefined
    ['label', 'submit', 'hidden_field']
  end

  def self.define_field_helpers
    (field_helpers + helpers_not_included_by_default - helpers_that_mustnt_be_redefined ).each do |name|
      create_labeled_field(name)
    end
  end

  define_field_helpers

end