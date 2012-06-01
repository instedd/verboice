module CallFlowHelper
  def link_to_add_call_flow(name, project, options={})
    new_call_flow = CallFlow.new
    new_call_flow.project = project
    fields = render "box", :call_flow => new_call_flow, :expanded => true
    link_to_function(name, "add_fields(this, \"#{escape_javascript(fields)}\")", options)
  end
end