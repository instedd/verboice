Command
XmlParser

# def constantize_all_in path
#   Dir[path].each do |file|
#       p file
#     if File.directory? file
#       constantize_all_in File.join(file, '*')
#     else
#       ActiveSupport::Inflector.camelize(file[file.rindex('/') + 1 .. -4]).constantize
#     end
#   end
# end

# Dir.glob("#{Rails.root}/app/models/**/*.rb").sort.each do |file|
#   ActiveSupport::Inflector.camelize(file[file.rindex('/') + 1 .. -4]).constantize
# end