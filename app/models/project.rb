class Project < ActiveRecord::Base

  belongs_to :account
  belongs_to :default_call_flow, :class_name => "CallFlow", :foreign_key => "call_flow_id"

  has_many :call_flows, :dependent => :destroy
  has_many :call_logs, :dependent => :destroy
  has_many :queued_calls, :dependent => :destroy
  has_many :external_services, :dependent => :destroy
  has_many :external_service_steps, :through => :external_services

  validates_presence_of :name
  validates_uniqueness_of :name, :scope => :account_id

  config_accessor :status_callback_url_user, :status_callback_url_password

  attr_encrypted :config, :key => ENCRYPTION_KEY, :marshal => true

  def call(address)
  end
end
