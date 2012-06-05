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

module CallFlowHelper
  def link_to_add_call_flow(name, project, options={})
    new_call_flow = CallFlow.new
    new_call_flow.project = project
    fields = render "box", :call_flow => new_call_flow, :expanded => true
    link_to_function(name, "add_fields(this, \"#{escape_javascript(fields)}\")", options)
  end
end