class Channel < ActiveRecord::Base
  Kinds = %w(sip2sip)

  belongs_to :account
  belongs_to :application

  validates_presence_of :account
  validates_presence_of :application

  after_commit :call_pbx_update_channel, :if => :persisted?
  before_destroy :call_pbx_delete_channel

  serialize :config, Hash

  def config
    read_attribute(:config) || {}
  end

  def host_and_port?
    config['host_and_port'].present?
  end

  def host_and_port
    config['host_and_port'].split ':', 2
  end

  def user
    config['user']
  end

  def password
    config['password']
  end

  def register?
    config['register'] == '1'
  end

  private

  def call_pbx_update_channel
    PbxClient.update_channel self.id
  end

  def call_pbx_delete_channel
    PbxClient.delete_channel self.id
  end
end
