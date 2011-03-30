class Channel < ActiveRecord::Base
  belongs_to :account
  belongs_to :application

  validates_presence_of :account
  validates_presence_of :application

  after_save :call_pbx_update_channel

  serialize :config, Hash

  def call_pbx_update_channel
    PbxClient.update_channel self.id
  end
end
