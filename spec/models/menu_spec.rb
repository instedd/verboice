require 'spec_helper'

describe Menu do

  it "should compile to a verboice equivalent flow" do
    menu = Menu.new
    #TODO
  end


  it "should be able to build itself from an incomming hash" do
    Menu.new {id: 27, type: :menu, data: {explanation_text: 'foo',  child: } }
  end
end
