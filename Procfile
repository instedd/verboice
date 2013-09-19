web: bundle exec rails s
broker: sh -c "cd broker && erl -pa ebin/ deps/*/ebin -boot start_sasl -s reloader -s main -config verboice -noinput +Bd"
asterisk: /usr/local/asterisk/sbin/asterisk
delayed: bundle exec rake jobs:work
