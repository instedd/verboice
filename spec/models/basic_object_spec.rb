require 'spec_helper'

describe BasicObject do

  it "should give all the subclasses of an object" do
    Foo.all_subclasses.size.should eq(4)
    Foo.all_subclasses.should include(Bar)
    Foo.all_subclasses.should include(Zzz)
    Foo.all_subclasses.should include(Zzzz)
    Foo.all_subclasses.should include(Asdf)
  end
  
  it "should give all leaf subclasses of an object" do
    Foo.all_leaf_subclasses.size.should eq(3)
    Foo.all_leaf_subclasses.should include(Zzz)
    Foo.all_leaf_subclasses.should include(Zzzz)
    Foo.all_leaf_subclasses.should include(Asdf)
  end

  it "should give the subclasses of an object" do
    Foo.subclasses.size.should eq(2)
    Foo.subclasses.should include(Bar)
    Foo.subclasses.should include(Asdf)
  end

end

class Foo
end
class Bar < Foo
end
class Zzz < Bar
end
class Zzzz < Bar
end
class Asdf < Foo
end