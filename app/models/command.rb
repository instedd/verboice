class Command
  attr_accessor :next

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

  def last
    n = self
    n = n.next while n.next
    n
  end

  def run(session)
    @next
  end

  def self.param(name, type, options = {})
    @spec << {:name => name, :type => type}.merge(options)
  end

  def ==(other)
    self.compare_to other
  end

  def compare_to(other, visited = Set.new)
    return false unless self.class == other.class
    visited.add self

    (instance_variables | other.instance_variables).each do |var|
      val = instance_variable_get var
      other_val = other.instance_variable_get var
      if val.is_a? Command
        next if visited.include? val
        return false unless val.compare_to(other_val, visited)
      else
        return false if val != other_val
      end
    end

    true
  end
end