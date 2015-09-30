Admin interface for Verboice
============================

Provides a simple web interface for performing and restoring backups in a Verboice instance.

Format
------

The backup format is a .tar.gz file with the following structure, where `verboice.sql` is a backup of the database and `call_logs` has the full content of the `record_dir` specified in the broker config.

```
  verboice-YYYYmmddTHHMMSS.tar.gz
  - verboice-YYYYmmddTHHMMSS.sql
  - call_logs
    - CALL_LOG_ID
      - results
        - RECORDING1.wav
        - RECORDING2.wav
```
