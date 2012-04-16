require 'spec_helper'

module Commands
  describe IfCommand do
    before(:each) do
      @session = Session.new :pbx => mock('pbx')
    end

    it "if variable true branch" do
      @session[:some_var] = true

      cmd = IfCommand.new :some_var, :first, :second
      cmd.run(@session).should == :first
    end

    it "if variable else branch" do
      @session[:some_var] = false

      cmd = IfCommand.new :some_var, :first, :second
      cmd.run(@session).should == :second
    end

    it "if variable true branch empty" do
      @session[:some_var] = true

      cmd = IfCommand.new :some_var, nil
      cmd.next = :next
      cmd.run(@session).should == :next
    end

    it "if variable else branch empty" do
      @session[:some_var] = false

      cmd = IfCommand.new :some_var, nil
      cmd.next = :next
      cmd.run(@session).should == :next
    end

    it "if variable with complex condition" do
      @session[:var1] = 1
      @session[:var2] = 2

      cmd = IfCommand.new "var1 + var2 == 3", :first
      cmd.run(@session).should == :first
    end
  end
end