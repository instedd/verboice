class ChannelsUiController < ApplicationController
  skip_before_filter :check_guisso_cookie
  skip_after_filter :intercom_rails_auto_include
  before_filter :authenticate_api_account!
  before_filter :new_channel, only: [:new, :create]
  layout 'channels_ui'

  def check_guisso_cookie
    true
  end

  def new
    load_config_from_channel() if @channel
  end

  def create
    load_config_to_channel()
    @channel.account = current_account

    unless @channel.save
      load_config_from_channel()
      render action: "new"
    end
  end

  def show
    @channel = current_account.channels.find_by_id(params[:id])
    load_config_from_channel()
  end

  def update
    @channel = current_account.channels.find_by_id(params[:id])
    load_config_to_channel()

    unless @channel.save
      load_config_from_channel()
      render action: "show"
    end
  end

  private

  def new_channel
    @channel =
      case params[:kind]
      when "callcentric", "nexmo", "skype"
        channel = Channels::TemplateBasedSip.new
        channel.kind = params[:kind].titleize
        channel
      when "sip_client"
        Channels::CustomSip.new
      when "twilio"
        Channels::Twilio.new
      when "africas_talking"
        Channels::AfricasTalking.new
      end
  end

  def load_config_from_channel
    @kind = @channel.api_kind
    @config =
      case @kind
      when "callcentric"
        ext =
          if @channel.number && @channel.username && @channel.number.starts_with?(@channel.username)
            @channel.number[@channel.username.length..-1]
          else
            ""
          end

        OpenStruct.new({
          name: @channel.name,
          number: @channel.username,
          ext: ext,
          password: @channel.password,
          limit: @channel.limit,
          errors: @channel.errors
        })

      when "nexmo", "skype"
        OpenStruct.new({
          name: @channel.name,
          username: @channel.username,
          password: @channel.password,
          number: @channel.number,
          limit: @channel.limit,
          errors: @channel.errors
        })

      when "sip_client"
        OpenStruct.new({
          name: @channel.name,
          username: @channel.username,
          password: @channel.password,
          number: @channel.number,
          limit: @channel.limit,
          domain: @channel.domain,
          register: @channel.register,
          errors: @channel.errors
        })

      when "twilio"
        OpenStruct.new({
          name: @channel.name,
          account_sid: @channel.account_sid,
          auth_token: @channel.auth_token,
          number: @channel.number,
          limit: @channel.limit,
          errors: @channel.errors
        })

      when "africas_talking"
        OpenStruct.new({
          name: @channel.name,
          username: @channel.username,
          api_key: @channel.api_key,
          number: @channel.number,
          limit: @channel.limit,
          errors: @channel.errors
        })
    end
  end

  def load_config_to_channel
    case params[:kind]
    when "callcentric"
      @channel.name = params[:config][:name]
      @channel.username = params[:config][:number]
      @channel.password = params[:config][:password]
      @channel.number = "#{params[:config][:number]}#{params[:config][:ext]}"
      @channel.limit = params[:config][:limit]
    when "nexmo", "skype"
      @channel.name = params[:config][:name]
      @channel.username = params[:config][:username]
      @channel.password = params[:config][:password]
      @channel.number = params[:config][:number]
      @channel.limit = params[:config][:limit]
    when "sip_client"
      @channel.name = params[:config][:name]
      @channel.username = params[:config][:username]
      @channel.password = params[:config][:password]
      @channel.number = params[:config][:number]
      @channel.limit = params[:config][:limit]
      @channel.domain = params[:config][:domain]
      @channel.register = params[:config][:register]
    when "twilio"
      @channel.name = params[:config][:name]
      @channel.account_sid = params[:config][:account_sid]
      @channel.auth_token = params[:config][:auth_token]
      @channel.number = params[:config][:number]
      @channel.limit = params[:config][:limit]
    when "africas_talking"
      @channel.name = params[:config][:name]
      @channel.username = params[:config][:username]
      @channel.api_key = params[:config][:api_key]
      @channel.number = params[:config][:number]
      @channel.limit = params[:config][:limit]
    end
  end
end
