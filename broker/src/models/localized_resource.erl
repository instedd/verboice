-module(localized_resource).
-export([prepare/2]).
-define(CACHE, true).
-define(TABLE_NAME, "localized_resources").
-include_lib("erl_dbmodel/include/model.hrl").

prepare(Session, #localized_resource{type = <<"TextLocalizedResource">>, text = Text, language = Language}) ->
  resource:prepare_text_resource(Text, binary_to_list(Language), Session);

prepare(Session, #localized_resource{type = <<"UploadLocalizedResource">>, uploaded_audio = Blob, guid = Guid}) ->
  resource:prepare_blob_resource(binary_to_list(Guid), Blob, Session);

prepare(Session, #localized_resource{type = <<"RecordLocalizedResource">>, recorded_audio = Blob, guid = Guid}) ->
  resource:prepare_blob_resource(binary_to_list(Guid), Blob, Session).
