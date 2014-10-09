module CallLogHelper
  def step_result(activity)
    step_result = activity.fields['step_result']
    step_data = activity.fields['step_data']
    step_data ? "#{step_result}:#{step_data}" : step_result
  end
end

