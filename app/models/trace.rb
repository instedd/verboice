class Trace < ActiveRecord::Base
  belongs_to :call_flow
  belongs_to :call_log, :foreign_key => "call_id"
end