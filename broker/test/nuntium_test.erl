-module(nuntium_test).
-include_lib("eunit/include/eunit.hrl").
-include("session.hrl").
-include("db.hrl").

should_work_with_text_localized_resource_test() ->
  Session = #session{call_log = #call_log{}, js_context = erjs_context:new(), address = <<"123">>, project = #project{account_id = 1}},
  meck:new(call_log, [stub_all]),
  meck:new(nuntium_api, [stub_all]),
  meck:new(resource),
  meck:expect(resource, prepare, [resource_guid, '_'], {text, en, "some text"}),

  {next, #session{}} = nuntium:run([{rcpt_type, caller}, {resource_guid, resource_guid}], Session),

  ?assert(meck:called(nuntium_api, send_ao, [[{from, <<"sms://verboice">>}, {to, <<"sms://123">>}, {body, "some text"}, {account_id, 1}]])),
  meck:unload().

should_send_to_a_value_recipient_test() ->
  Session = #session{call_log = #call_log{}, js_context = erjs_context:new(), address = <<"123">>, project = #project{account_id = 1}},
  meck:new(call_log, [stub_all]),
  meck:new(nuntium_api, [stub_all]),
  meck:new(resource),
  meck:expect(resource, prepare, [resource_guid, '_'], {text, en, "some text"}),

  {next, #session{}} = nuntium:run([{rcpt_type, expr}, {resource_guid, resource_guid}, {expr, "555"}], Session),

  ?assert(meck:called(nuntium_api, send_ao, [[{from, <<"sms://verboice">>}, {to, <<"sms://555">>}, {body, "some text"}, {account_id, 1}]])),
  meck:unload().
