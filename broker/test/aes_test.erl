-module(aes_test).
-include_lib("eunit/include/eunit.hrl").

decrypt_test() ->
  ?assertEqual(<<"foo">>, aes:decrypt("secret", "i\x1AM+\xC3\x97\xCD6\x0E\x91Y<\x03j\xA1\x86")).
