-module(remember_impersonate).
-export([run/2]).
-include("session.hrl").
-include("db.hrl").

run(_Args, Session = #session{contact = Contact, call_flow = CallFlow}) ->
  CallFlowId = CallFlow#call_flow.id,
  ContactId = Contact#contact.id,
  ImpersonateRecord = impersonate_record:find([{call_flow_id, CallFlowId}, {contact_id, ContactId}]),
  case ImpersonateRecord of
    undefined -> {next, Session};
    _ ->
      ImpersonatedContact = contact:find(ImpersonateRecord#impersonate_record.impersonated_id),
      case ImpersonatedContact of
        undefined -> {next, Session};
        _ ->
          {next, impersonate:impersonate(Session, ImpersonatedContact)}
      end
  end.
