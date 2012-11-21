unless Rails.env == 'test'
  %w(sox lame).each do |program|
    unless `which #{program}` && $?.success?
      puts "Error: #{program} not found. Please install #{program}."
      exit(1)
    end
  end
end
