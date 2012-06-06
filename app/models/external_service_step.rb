class ExternalServiceStep < ActiveRecord::Base
  belongs_to :external_service

  attr_accessible :callback_url, :display_name, :icon, :name, :kind, :variables

  serialize :variables, Array
  serialize :response_variables, Array

  validates :name, :presence => true, :uniqueness => { :scope => :external_service_id }

  class Variable < Struct.new(:name, :display_name, :type)
  end

end
