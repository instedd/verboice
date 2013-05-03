-module(localized_resource).
-export([prepare/2]).
-define(TABLE_NAME, "localized_resources").
-include("model.hrl").

prepare(Session, #localized_resource{type = <<"TextLocalizedResource">>, text = Text}) ->
  resource:prepare_text_resource(Text, Session);

prepare(Session, #localized_resource{type = <<"UploadLocalizedResource">>, uploaded_audio = Blob, guid = Guid}) ->
  resource:prepare_blob_resource(binary_to_list(Guid), Blob, Session).