-module(verboice_telemetry).
-export([track_call_finished/1]).
-compile([{parse_transform, lager_transform}]).
-include("session.hrl").
-include("db.hrl").

track_call_finished(#session{project = Project = #project{account_id = AccountId}}) ->
  try
    Account = account:find(AccountId),
    {datetime, AccountCreatedAt} = Account#account.created_at,
    {datetime, ProjectCreatedAt} = Project#project.created_at,

    telemetry:report(timespan_update, [
      <<"account_lifespan">>,
      {[{<<"account_id">>, AccountId}]},
      iso8601:format(AccountCreatedAt)
    ]),
    telemetry:report(timespan_update, [
      <<"project_lifespan">>,
      {[{<<"project_id">>, Project#project.id}]},
      iso8601:format(ProjectCreatedAt)
    ])
  catch
    Class:Error -> lager:warning("An error occurred tracking call with telemetry: ~p", [{Class, Error}])
  end;


track_call_finished(_) ->
  ok.