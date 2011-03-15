class HomeController < BaseController
  before_filter :authenticate_account!

  def index
    @applications = current_account.applications
  end
end
