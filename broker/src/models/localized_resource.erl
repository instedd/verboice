-module(localized_resource).
-export([prepare/2]).
-define(CACHE, true).
-define(TABLE_NAME, "localized_resources").
-include_lib("erl_dbmodel/include/model.hrl").

prepare(_, #localized_resource{type = <<"TextLocalizedResource">>, text = undefined, language = Language}) ->
  throw(io_lib:format("Missing resource text for language '~s'", [Language]));

prepare(Session, #localized_resource{type = <<"TextLocalizedResource">>, text = Text, language = Language}) ->
  resource:prepare_text_resource(Text, binary_to_list(Language), Session);

prepare(Session, #localized_resource{type = <<"UploadLocalizedResource">>, uploaded_audio = Blob, guid = Guid, updated_at = {datetime, UpdatedAt}}) ->
  resource:prepare_blob_resource(binary_to_list(Guid), UpdatedAt, Blob, Session);

prepare(Session, #localized_resource{type = <<"RecordLocalizedResource">>, recorded_audio = Blob, guid = Guid, updated_at = {datetime, UpdatedAt}}) ->
  resource:prepare_blob_resource(binary_to_list(Guid), UpdatedAt, Blob, Session);

prepare(Session, #localized_resource{type = <<"UrlLocalizedResource">>, url = Url}) ->
  resource:prepare_url_resource(binary_to_list(Url), Session).
