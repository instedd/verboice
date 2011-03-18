Thread.new { EM.run }

EM.error_handler do |err|
  p err
end
