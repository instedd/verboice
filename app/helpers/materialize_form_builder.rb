class MaterializeFormBuilder < ActionView::Helpers::FormBuilder
  def text_field(name, args = {})
    input_field(name, args) do
      super(name, args)
    end
  end

  def password_field(name, args = {})
    input_field(name, args) do
      super(name, args)
    end
  end

  def check_box(name, args = {})
    base_input_field(name, args) do
      @template.content_tag(:label) do
        super(name, args) +
        @template.content_tag(:span, args[:label])
      end
    end
  end

  def has_error?(name)
    object.respond_to?(:errors) && object.errors[name].present?
  end

  def error_message(name)
    object.errors[name]
      .map { |message| object.errors.full_message(name, message) }
      .join(', ')
  end

  def helper(name, args)
    if has_error?(name) || args[:helper].present?
      error = error_message(name)
      @template.content_tag(:span, args[:helper], { class: 'helper-text', 'data-error': error })
    end
  end

  def base_input_field(name, args)
    original_proc = ActionView::Base.field_error_proc
    ActionView::Base.field_error_proc = proc { |input, instance| input }

    colspan = args[:colspan] || 's12'
    @template.content_tag(:div, { class: "input-field col #{colspan}" }) do
      args[:class] = 'invalid' if has_error?(name)
      yield
    end

  ensure
    ActionView::Base.field_error_proc = original_proc
  end

  def input_field(name, args)
    base_input_field(name, args) do
      yield +
      label(name, args[:label]) +
      helper(name, args)
    end
  end
end
