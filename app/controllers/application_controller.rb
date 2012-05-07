class ApplicationController < ActionController::Base
  protect_from_forgery

  before_filter do
    @body_class = ['full-width']
  end
end
