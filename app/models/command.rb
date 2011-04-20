class Command
  def self.inherited(subclass)
    @@commands ||= []
    @@commands << subclass
    subclass.instance_eval { @spec = [] }
  end

  def self.specs
    @@commands.inject({}) do |hash, cmd|
      hash[cmd.name[0 .. -8].downcase] = cmd.spec
      hash
    end
  end

  def self.spec
    @spec
  end

  def self.param(name, type, options = {})
    @spec << {:name => name, :type => type}.merge(options)
  end
end

Dir["#{Rails.root}/app/models/commands/*"].each do |file|
  ActiveSupport::Inflector.camelize(file[file.rindex('/') + 1 .. -4]).constantize
end
