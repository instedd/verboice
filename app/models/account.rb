class Account < ActiveRecord::Base
  # Include default devise modules. Others available are:
  # :token_authenticatable, :confirmable, :lockable and :timeoutable
  devise :database_authenticatable, :registerable,
         :recoverable, :rememberable, :trackable, :validatable

  # Setup accessible (or protected) attributes for your model
  attr_accessible :email, :password, :password_confirmation, :remember_me

  has_many :applications, :dependent => :destroy
  has_many :channels, :dependent => :destroy
  has_many :call_logs
  has_many :call_queues

  has_many :queued_calls, :through => :channels

  def call(options = {})
    channel = channels.find_by_name! options[:channel]
    channel.call options[:address], options
  end
end
