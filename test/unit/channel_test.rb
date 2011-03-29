require 'test_helper'

class ChannelTest < ActiveSupport::TestCase
  should belong_to(:account)
  should belong_to(:application)

  should validate_presence_of(:account)
  should validate_presence_of(:application)
end
