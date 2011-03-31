class Channel < ActiveRecord::Base
  belongs_to :account
  belongs_to :application

  validates_presence_of :account
  validates_presence_of :application

  after_commit :call_pbx_update_channel
  before_destroy :call_pbx_delete_channel

  serialize :config, Hash

  private

  def call_pbx_update_channel
    PbxClient.update_channel self.id
  end

  def call_pbx_delete_channel
    PbxClient.delete_channel self.id
  end
end
