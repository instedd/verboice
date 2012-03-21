class PlayUrlCommand < Command
  include PlayCommand
  param :url, :string, :ui_length => 80

  def initialize(url)
    @url = url
    super Digest::MD5.hexdigest @url
  end

  def run(session)
    session.info "Play #{@url}"
    super
  end

  def setup_file(session)
    target_path = get_target_path(session)
    session.trace "Download #{@url}"
    download_url_to target_path
    target_path
  end
end
