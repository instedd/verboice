class OAuthToken < ActiveRecord::Base
  attr_accessible :access_token, :expires_at, :refresh_token, :service

  belongs_to :account

  validates_presence_of :account
  validates_uniqueness_of :service, :scope => :account_id

  def self.new_from(oauth_response, a_service)
    self.new :access_token => oauth_response.token,
      :refresh_token => oauth_response.refresh_token,
      :expires_at => DateTime.now.utc + oauth_response.expires_in.to_i.seconds,
      :service => a_service
  end

  enumerated_attribute :service, %w(^google)
end
