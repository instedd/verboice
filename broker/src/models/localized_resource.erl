-module(localized_resource).
-export([prepare/2]).
-define(TABLE_NAME, "localized_resources").
-include("model.hrl").

prepare(Pbx, #localized_resource{type = <<"TextLocalizedResource">>, text = Text}) ->
  resource:prepare_text_resource(Text, Pbx);

prepare(Pbx, #localized_resource{type = <<"UploadLocalizedResource">>, uploaded_audio = Blob, guid = Guid}) ->
  resource:prepare_blob_resource(binary_to_list(Guid), Blob, Pbx).