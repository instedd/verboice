class ExternalService < ActiveRecord::Base
  belongs_to :project
  attr_accessible :name, :url, :xml
end
