class ApiChannelsController < ApplicationController
  before_filter :authenticate_account!
  skip_before_filter :verify_authenticity_token

  def create
    data = request.raw_post
    data = JSON.parse(data).with_indifferent_access
    channel = Channel.from_json data
    channel.account = current_account
    channel.application = current_account.applications.find_by_name data[:application]
    if channel.save
      render :json => channel
    else
      render :json => errors_to_json(channel.errors, 'creating')
    end
  end

  def destroy
    chan = current_account.channels.find_by_name params[:name]

    return head :not_found unless chan

    chan.destroy
    head :ok
  end

  def list
    channel_names = current_account.channels.map &:name
    render :json => channel_names
  end

  private

  def errors_to_json(errors, action)
    attrs = {
      :summary => "There were problems #{action} the channel",
      :properties => []
    }
    errors.each do |name, value|
      attrs[:properties] << { name => value }
    end
    attrs
  end
end
