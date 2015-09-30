-module(impersonate).
-export([run/2, impersonate/2]).
-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{js_context = JS, project = #project{id = ProjectId}}) ->
  Variable = proplists:get_value(variable, Args),
  ValueExpr = proplists:get_value(value, Args),
  {Value, JS2} = erjs:eval(ValueExpr, JS),

  #project_variable{id = ProjectVarId} = project_variable:find([{project_id, ProjectId}, {name, Variable}]),
  NewSession = case persisted_variable:find([{project_variable_id, ProjectVarId}, {value, Value}]) of
    undefined ->
      poirot:log(info, "Could not impersonate. Contact not found with ~p = ~p", [Variable, Value]),
      Session#session{js_context = erjs_context:set(impersonated, false, JS2)};

    #persisted_variable{contact_id = ContactId} ->
      Contact = contact:find(ContactId),
      Addresses = [util:to_string(Address#contact_address.address) || Address <- contact_address:find_all([{contact_id, ContactId}])],
      poirot:log(info, "Impersonating as contact id: ~p, addresses: ~p", [Contact:id(), Addresses]),
      record_if_needed(Args, Session, Contact),
      impersonate(Session, Contact)
  end,
  {next, NewSession}.

impersonate(Session, Contact) ->
  CallLog = Session#session.call_log,
  CallLog:update([{contact_id, Contact#contact.id}]),
  NewSession = Session#session{contact = Contact},
  Context = session:default_variables(NewSession),
  Context2 = erjs_context:set(impersonated, true, Context),
  NewSession#session{js_context = Context2}.

record_if_needed(Args, _Session = #session{contact = Contact, call_flow = CallFlow}, ImpersonatedContact) ->
  Remember = proplists:get_value(remember, Args),
  case Remember of
    true ->
      CallFlowId = CallFlow#call_flow.id,
      ContactId = Contact#contact.id,
      ImpersonateRecord = impersonate_record:find_or_create([
        {call_flow_id, CallFlowId},
        {contact_id, ContactId}
      ]),
      ImpersonateRecord:update([{impersonated_id, ImpersonatedContact#contact.id}]);
    _ -> ok
  end.
