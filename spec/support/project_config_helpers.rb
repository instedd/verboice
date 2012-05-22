module ProjectConfigHelpers
  def with_callback_url_accessors
    [nil, :_user, :_password].each do |accessor|
      yield("status_callback_url#{accessor}".to_sym, accessor == :_password ? :password : :text)
    end
  end
end
