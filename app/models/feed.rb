class Feed < ActiveRecord::Base
  attr_accessible :name, :key, :project
  belongs_to :project
  before_save :generate_key, unless: :key?

  def generate_key
    self.key = Guid.new.to_s
  end
end
