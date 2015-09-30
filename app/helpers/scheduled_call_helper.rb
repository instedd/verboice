module ScheduledCallHelper

  def link_to_add_scheduled_call(name, project, options = {})
    scheduled_call = ScheduledCall.new(project: project)
    fields = render "box", scheduled_call: scheduled_call, expanded: true
    link_to_function(name, "add_scheduled_call_box(this, \"#{escape_javascript(fields)}\")", options)
  end

end
