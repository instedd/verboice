

class Librevox::Listener::Base

  def handle_response_with_command_reply
    if response.command_reply? && @command_queue.any?
      @command_queue.shift.resume response
    else
      handle_response_without_command_reply
    end
  end

  alias_method_chain :handle_response, :command_reply

end