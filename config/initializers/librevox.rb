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
