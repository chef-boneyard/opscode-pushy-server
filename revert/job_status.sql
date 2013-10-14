-- Revert job_status

BEGIN;

DROP TABLE IF EXISTS job_status;

COMMIT;
