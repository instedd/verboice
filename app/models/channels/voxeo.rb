class Channels::Voxeo < Channel

  validates_numericality_of :limit, :only_integer => true, :greater_than => 0, :if => :has_limit?

  config_accessor :token
  config_accessor :url
  config_accessor :limit

  attr_protected :guid

  before_create :create_guid

  def port
    Rails.configuration.verboice_configuration[:voxeo_broker_port].to_i
  end

  def create_guid
    self.guid ||= Guid.new.to_s
  end

  def self.can_handle? a_kind
    a_kind == 'voxeo'
  end
end
