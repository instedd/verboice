class ExternalService < ActiveRecord::Base
  belongs_to :project
  has_many :external_service_steps, :autosave => true, :dependent => :destroy

  attr_accessible :name, :url, :xml

  def update_manifest!
    response = RestClient.get self.url
    self.xml = response.to_str
    self.save!
  end

  def update_from_manifest(xml)
    Parsers::ExternalService.new(self).parse(xml).save
  end

  def self.create_from_manifest(xml)
    Parsers::ExternalService.new.parse(xml).tap do |service|
      service.save
    end
  end

  def steps
    external_service_steps
  end

end
