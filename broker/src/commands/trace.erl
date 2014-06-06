-module(trace).
-export([run/2]).
-include("session.hrl").

run(Args, Session = #session{js_context = JS}) ->
  % CallFlowId = proplists:get_value(call_flow_id, Args),
  % StepId = proplists:get_value(step_id, Args),
  % StepName = proplists:get_value(step_name, Args),
  Expression = proplists:get_value(expression, Args),
  {Result, _} = erjs:eval(Expression, JS),
  % CallLog:trace_record(CallFlowId, StepId, StepName, Result),
  poirot:log(info, Result),
  {next, Session}.
