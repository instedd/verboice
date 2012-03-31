class BasicObject

  def all_leaf_subclasses
    all_subclasses.select do |a_class|
      a_class.subclasses.empty?
    end
  end
  
	def all_subclasses

	  #   This is the equivalent of doing:
    #   def all_subclasses
	  #      scan = subclasses.clone
	  #      subclasses.each do |a_subclass|
	  #        scan << a_subclass.all_subclasses
	  #      end
	  #      scan.flatten
	  #    end
    #   But faster...
    
  	scan = subclasses.clone
  	index = 0
  	while (index < scan.size) do
  	  scan << scan[index].subclasses
		  scan.flatten!
		  index += 1
		end
  	scan
	end
end