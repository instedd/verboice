require 'spec_helper'

describe IfCommand do
  before(:each) do
    @session = Session.new :pbx => mock('pbx')
  end

  it "if variable true branch" do
    @session[:some_var] = true
    @session.should_receive(:push_commands).with([:first])

    cmd = IfCommand.new :condition => :some_var, :then => [:first], :else => [:second]
    cmd.run @session
  end

  it "if variable else branch" do
    @session[:some_var] = false
    @session.should_receive(:push_commands).with([:second])

    cmd = IfCommand.new :condition => :some_var, :then => [:first], :else => [:second]
    cmd.run @session
  end

  it "if variable true branch not an array" do
    @session[:some_var] = true
    @session.should_receive(:push_commands).with([:first])

    cmd = IfCommand.new :condition => :some_var, :then => :first, :else => [:second]
    cmd.run @session
  end

  it "if variable false branch not an array" do
    @session[:some_var] = false
    @session.should_receive(:push_commands).with([:second])

    cmd = IfCommand.new :condition => :some_var, :then => [:first], :else => :second
    cmd.run @session
  end

  it "if variable true branch empty" do
    @session[:some_var] = true

    cmd = IfCommand.new :condition => :some_var
    cmd.run @session
  end

  it "if variable else branch empty" do
    @session[:some_var] = false

    cmd = IfCommand.new :condition => :some_var
    cmd.run @session
  end

  it "if variable with complex condition" do
    @session[:var1] = 1
    @session[:var2] = 2
    @session.should_receive(:push_commands).with([:first])

    cmd = IfCommand.new :condition => "var1 + var2 == 3", :then => [:first]
    cmd.run @session
  end
end
