class Command
  def self.inherited(subclass)
    subclass.instance_eval { @spec = [] }
  end

  def self.specs
    subclasses.inject({}) do |hash, cmd|
      hash[cmd.name[0 .. -8].underscore] = cmd.spec
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