module WorkflowHelper

  def store_value_tags
    content_tag(:input, '', :type => 'checkbox', 'data-bind' => 'checked: defines_store') +\
    content_tag(:span, "Store this result as: ") +\
    content_tag(:input, '', :type => 'text', 'data-bind' => 'value: store, enable: defines_store', :style => "width: 110px")
  end

end