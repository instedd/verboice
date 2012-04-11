class Trace < ActiveRecord::Base
  belongs_to :application
  belongs_to :call_log, :foreign_key => "call_id"
end