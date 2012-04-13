require 'spec_helper'

describe ApplicationsController do
  include Devise::TestHelpers

  before(:each) do
    @account = Account.make
    sign_in @account
  end


  it 'Should retrieve a csv with the call traces' do
    app = Application.make id: 3, user_flow: [{"id"=>1, "name"=>"Initial menu", "type"=>"menu", "root"=>true, "options"=>[{"number"=>1, "next"=>593}, {"number"=>2, "next"=>737}], "end_call_message"=>{"name"=>"Bye", "type"=>"record", "duration"=>"00:00"}, "invalid_message"=>{"name"=>"Wrong number!", "type"=>"record", "duration"=>"00:00"}, "explanation_message"=>{"name"=>"Welcome to test app 01", "type"=>"record", "duration"=>"00:00"}, "options_message"=>{"name"=>"Press 1 for foo, press 2 for bar", "type"=>"record", "duration"=>"00:00"}}, {"id"=>593, "name"=>"Menu Foo", "type"=>"menu", "root"=>false, "options"=>[{"number"=>1, "next"=>509}, {"number"=>2, "next"=>897}], "end_call_message"=>{}, "invalid_message"=>{}, "explanation_message"=>{"name"=>"You pressed Foo", "type"=>"record", "duration"=>"00:00"}, "options_message"=>{"name"=>"Press 1 if it's ok, if not, 2", "type"=>"record", "duration"=>"00:00"}}, {"id"=>737, "name"=>"Menu Bar", "type"=>"menu", "root"=>false, "options"=>[{"number"=>2, "next"=>2}, {"number"=>3, "next"=>3}], "end_call_message"=>{"name"=>"Goodbye", "type"=>"record", "duration"=>"00:00"}, "invalid_message"=>{}, "explanation_message"=>{"name"=>"You chosed Bar", "type"=>"record", "duration"=>"00:00"}, "options_message"=>{"name"=>"Press 2 if it's ok, 3 if not", "type"=>"record", "duration"=>"00:00"}}, {"id"=>509, "name"=>"Menu Ok", "type"=>"menu", "root"=>false, "options"=>[], "end_call_message"=>{"name"=>"Goodbye", "type"=>"record", "duration"=>"00:00"}, "invalid_message"=>{}, "explanation_message"=>{"name"=>"Thank you for chosing foo", "type"=>"record", "duration"=>"00:00"}, "options_message"=>{}}, {"id"=>897, "name"=>"Menu not ok", "type"=>"menu", "root"=>false, "options"=>[], "end_call_message"=>{"name"=>"Bye", "type"=>"record", "duration"=>"00:00"}, "invalid_message"=>{}, "explanation_message"=>{"name"=>"We will call you back later", "type"=>"record", "duration"=>"00:00"}, "options_message"=>{}}, {"id"=>2, "name"=>"Menu Ok", "type"=>"menu", "root"=>false, "options"=>[], "end_call_message"=>{}, "invalid_message"=>{}, "explanation_message"=>{"name"=>"Ok. Thank you for choosing bar", "type"=>"record", "duration"=>"00:00"}, "options_message"=>{}}, {"id"=>3, "name"=>"Menu Not ok", "type"=>"menu", "root"=>false, "options"=>[], "end_call_message"=>{}, "invalid_message"=>{}, "explanation_message"=>{"name"=>"Goodbye", "type"=>"record", "duration"=>"00:00"}, "options_message"=>{}}], account_id: @account.id

    call_log1 = CallLog.make id: 1, address: 1000, application: app
    call_log2 = CallLog.make id: 2, address: 1000, application: app
    call_log3 = CallLog.make id: 3, address: 1000, application: app
    call_log4 = CallLog.make id: 4, address: 1000, application: app
    call_log5 = CallLog.make id: 5, address: 1000, application: app
    call_log6 = CallLog.make id: 6, address: 1000, application: app
    Trace.make application: app, call_log: call_log3, step_id: 1, result: "No key was pressed. Timeout.", step_name: 'Initial menu'
    Trace.make application: app, call_log: call_log1, step_id: 1, result: "User pressed: 2"
    Trace.make application: app, call_log: call_log2, step_id: 1, result: "User pressed: 1"
    Trace.make application: app, call_log: call_log2, step_id: 593, result: "User pressed: 1"
    Trace.make application: app, call_log: call_log4, step_id: 1, result: "User pressed: 1"
    Trace.make application: app, call_log: call_log4, step_id: 593, result: "User pressed: 2"
    Trace.make application: app, call_log: call_log5, step_id: 1, result: "User pressed: 2"
    Trace.make application: app, call_log: call_log5, step_id: 737, result: "No key was pressed. Timeout.", step_name: "Menu Bar"
    Trace.make application: app, call_log: call_log6, step_id: 1, result: "User pressed: 2", step_name: "Menu inicial"
    Trace.make application: app, call_log: call_log6, step_id: 737, result: "User pressed: 2", step_name: "Menu Bar"

    response = get :show, :format => :csv, id: app.id
    response.body.should eq File.read(File.join(Rails.root, 'spec/fixtures/trace.csv'))
  end
end
