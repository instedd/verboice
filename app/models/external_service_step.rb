class ExternalServiceStep < ActiveRecord::Base
  belongs_to :external_service

  attr_accessible :callback_url, :display_name, :icon, :name, :type, :variables

  serialize :variables, Array

  validates :name, :presence => true, :uniqueness => { :scope => :external_service_id }

  class Variable
    attr_accessor :name, :display_name, :type
  end

end
