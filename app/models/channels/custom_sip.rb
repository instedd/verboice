class Channels::CustomSip < Channels::Sip

  validates_numericality_of :limit, :only_integer => true, :greater_than => 0, :if => :has_limit?

  config_accessor :host
  config_accessor :register
  config_accessor :direction
  config_accessor :limit

  def servers
    hosts = config['host'] || []
    servers = []
    hosts.each_with_index do |host, i|
      servers << Server.new(host, config['register'][i], config['direction'][i])
    end
    servers.length == 0 ? [Server.new] : servers
  end

  def self.can_handle? a_kind
    a_kind == 'sip'
  end

  def self.kind
    'Sip'
  end

  def server_username_uniqueness
    conflicting_channels = Channels::CustomSip.all.select{|c| c.username == self.username && c.id != self.id && !(c.host & self.host).empty?}
    errors.add(:base, 'Username and host have already been taken') unless conflicting_channels.empty?
  end

end