class ChannelsController < ApplicationController
  before_filter :authenticate_account!

  # GET /channels
  def index
    @channels = current_account.channels.includes(:project).all
  end

  # GET /channels/1
  def show
    @channel = current_account.channels.find(params[:id])
  end

  # GET /channels/new
  def new
    @channel = current_account.channels.new :kind => params[:kind]
  end

  # GET /channels/1/edit
  def edit
    @channel = current_account.channels.find(params[:id])
  end

  # POST /channels
  def create
    @channel = current_account.channels.new(params[:channel])

    if @channel.save
      redirect_to(channels_path, :notice => "Channel #{@channel.name} successfully created.")
    else
      render :action => "new"
    end
  end

  # PUT /channels/1
  def update
    @channel = current_account.channels.find(params[:id])

    if @channel.update_attributes(params[:channel])
      redirect_to(channels_path, :notice => "Channel #{@channel.name} successfully updated.")
    else
      render :action => "edit"
    end
  end

  # DELETE /channels/1
  def destroy
    @channel = current_account.channels.find(params[:id])
    @channel.destroy

    redirect_to(channels_url)
  end

  def call
    @channel = current_account.channels.find(params[:id])
    render :layout => false
  end
end
