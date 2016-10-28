# Verboice specs

The command `rspec spec` will run Ruby unit and functional tests. For running the broker's tests, see the `eunit` task in the broker's makefile.

To run the integration specs, first start up the local Vagrant boxes for asterisk; note that Asterisk may fail to load some modules on startup, if that's the case, issue a restart order. Then, start the broker in integration mode. Make sure there is no asterisk running locally.

```bash
cd spec/support/etc
vagrant up
vagrant ssh remote -c "sudo asterisk -x \"core restart now\""
vagrant ssh verboice -c "sudo asterisk -x \"core restart now\""
cd -
make -C broker run-integration
```

As with the standard specs, the test database must be created and up to date, and elasticsearch needs to be running.

Run `rspec spec -t integration` to run the integration specs. Total running time is about 3 minutes.
