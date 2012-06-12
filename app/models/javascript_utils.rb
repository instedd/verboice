module JavascriptUtils
  include ActionView::Helpers::JavaScriptHelper

  def value_for_js(value)
    is_numeric?(value) ? value : "'#{escape_javascript value}'"
  end

  def is_numeric?(obj)
     obj.to_s.match(/\A\d+?(\.\d+)?\Z/) == nil ? false : true
  end

end