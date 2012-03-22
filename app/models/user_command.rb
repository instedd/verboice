class UserCommand

  # Dir["#{Rails.root}/app/models/user_commands/*"].each do |file|
  #   ActiveSupport::Inflector.camelize(file[file.rindex('/') + 1 .. -4]).constantize
  # end

  def self.all
    subclasses
  end

end