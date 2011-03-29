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

  def call(options = {})
    if options[:application]
      application = applications.find options[:application]
    elsif options[:callback]
      application = applications.find_or_create_by_callback_url options[:callback]
    end

    application.call options[:address]
  end
end
