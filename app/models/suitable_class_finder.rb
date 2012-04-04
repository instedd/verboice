class SuitableClassFinder

  attr_reader :collaborators, :classes

  def initialize a_collection_of_classes , params = {}
  	@classes = a_collection_of_classes
  	@testing_message = params[:sending] || self.class.default_can_handle_message
  	@collaborators = if params[:suitable_for].is_an? Array
  	  params[:suitable_for]
	  else
  	  [params[:suitable_for]]
	  end
  	@if_none_do_block = params[:if_none] || self.class.default_if_none_block
  	@if_multiple_do_block = params[:if_multiple] || self.class.default_if_multiple_block
  end

  def self.default_can_handle_message
  	:can_handle?
  end

  def self.find_direct_subclass_of an_abstract_class, params
    find_in an_abstract_class.subclasses, params
  end

  def self.find_leaf_subclass_of an_abstract_class, params
    find_in an_abstract_class.all_leaf_subclasses, params
  end

  def self.find_any_subclass_of an_abstract_class, params
    find_in an_abstract_class.all_subclasses, params
  end
  
  def self.find_in a_list_of_classes, params
    self.new a_list_of_classes, params
  end

  def self.default_if_multiple_block
  	lambda { |potential_classes, suitable_class_finder |
    	raise "There should not be more than one class that could work with this objects." +
    	" The classes #{potential_classes.inspect} can work with #{suitable_class_finder.collaborators.inspect}." +
    	" This is a programming error."
  	}
  end

  def self.default_if_none_block
  	lambda { | suitable_class_finder |
  	  raise "None of the classes #{suitable_class_finder.classes.inspect} can work with #{suitable_class_finder.collaborators.inspect}." +
  	  " This is a programming error."
  	}
  end
	
	def value
  	suitable_classes = @classes.select do |a_class|
      a_class.send @testing_message, *@collaborators
  	end
  	
  	if suitable_classes.size == 1
  		suitable_classes.first
		else
  		if suitable_classes.empty?
  		  @if_none_do_block.call self
		  else
		    @if_multiple_do_block.call suitable_classes, self
	    end
    end
  end
end