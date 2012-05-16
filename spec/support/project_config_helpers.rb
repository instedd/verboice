module ProjectConfigHelpers
  def with_callback_url_accessors
    [nil, :status_].each do |type|
      [nil, :_user, :_password].each do |accessor|
        yield("#{type}callback_url#{accessor}".to_sym, accessor == :_password ? :password : :text)
      end
    end
  end
end
