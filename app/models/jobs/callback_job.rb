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
  rescue Exception => ex
    Delayed::Job.enqueue Jobs::CallbackJob.new(@url, @method, @body), run_at: 15.minutes.from_now
  end
end