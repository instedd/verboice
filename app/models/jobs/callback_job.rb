class Jobs::CallbackJob
  def initialize(url, method, body)
    @url = url
    @method = method
    @body = body
  end

  def perform
    if @method.to_s == 'get'
      RestClient.get @url, {:params => @body}
    else
      RestClient.post @url, @body
    end
  end
end