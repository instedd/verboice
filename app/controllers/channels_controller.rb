class ChannelsController < ApplicationController
  before_filter :authenticate_account!

  # GET /channels
  # GET /channels.xml
  def index
    @channels = current_account.channels.all

    respond_to do |format|
      format.html # index.html.erb
      format.xml  { render :xml => @channels }
    end
  end

  # GET /channels/1
  # GET /channels/1.xml
  def show
    @channel = current_account.channels.find(params[:id])

    respond_to do |format|
      format.html # show.html.erb
      format.xml  { render :xml => @channel }
    end
  end

  # GET /channels/new
  # GET /channels/new.xml
  def new
    @channel = current_account.channels.new

    respond_to do |format|
      format.html # new.html.erb
      format.xml  { render :xml => @channel }
    end
  end

  # GET /channels/1/edit
  def edit
    @channel = current_account.channels.find(params[:id])
  end

  # POST /channels
  # POST /channels.xml
  def create
    @channel = current_account.channels.new(params[:channel])

    respond_to do |format|
      if @channel.save
        format.html { redirect_to(@channel, :notice => 'Channel was successfully created.') }
        format.xml  { render :xml => @channel, :status => :created, :location => @channel }
      else
        format.html { render :action => "new" }
        format.xml  { render :xml => @channel.errors, :status => :unprocessable_entity }
      end
    end
  end

  # PUT /channels/1
  # PUT /channels/1.xml
  def update
    @channel = current_account.channels.find(params[:id])

    respond_to do |format|
      if @channel.update_attributes(params[:channel])
        format.html { redirect_to(@channel, :notice => 'Channel was successfully updated.') }
        format.xml  { head :ok }
      else
        format.html { render :action => "edit" }
        format.xml  { render :xml => @channel.errors, :status => :unprocessable_entity }
      end
    end
  end

  # DELETE /channels/1
  # DELETE /channels/1.xml
  def destroy
    @channel = current_account.channels.find(params[:id])
    @channel.destroy

    respond_to do |format|
      format.html { redirect_to(channels_url) }
      format.xml  { head :ok }
    end
  end
end
