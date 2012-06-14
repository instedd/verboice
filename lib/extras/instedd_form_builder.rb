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