class ExternalService < ActiveRecord::Base
  belongs_to :project
  attr_accessible :name, :url, :xml

  def update_manifest!
    response = RestClient.get self.url
    self.xml = response.to_str
    self.save!
  end
end
