

class Librevox::Listener::Inbound

  def handle_response_with_command_reply
    if response.command_reply? && @command_queue.any?
      @command_queue.shift.resume response
    else
      handle_response_without_command_reply
    end
  end

  def unbind
    @command_queue.each do |cmd|
      cmd.resume Exception.new 'Error executing command in PBX'
    end
  end

  alias_method_chain :handle_response, :command_reply

end

class Librevox::Response

  attr_accessor :body

  def content_with_body=(content)
    ignore = true
    @body = ""
    content.each do |line|
      if ignore
        ignore = false if line.strip == ""
      else
        @body << line.strip
      end
    end

    self.content_without_body = content
  end

  alias_method :content_without_body=, :content=
  alias_method :content=, :content_with_body=

end
