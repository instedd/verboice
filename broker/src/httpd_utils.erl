-module(httpd_utils).
-export([if_not_already_handled/2]).

if_not_already_handled(Data, Handler) ->
  case proplists:get_value(status, Data) of
    {_StatusCode, _PhraseArgs, _Reason} ->
      {proceed, Data};
    undefined ->
      case proplists:get_value(response, Data) of
        undefined ->
          Handler();
        _Response ->
          {proceed, Data}
      end
  end.
