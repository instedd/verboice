# Load all xml parsers
Dir["#{Rails.root}/app/models/parsers/*"].each do |file|
  eval(ActiveSupport::Inflector.camelize(file[file.rindex('/') + 1 .. -4]))
end
