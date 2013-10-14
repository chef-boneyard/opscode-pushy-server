-- Verify jobs

BEGIN;

SELECT id, org_id, command, status, run_timeout, last_updated_by, created_at, updated_at, quorum
FROM jobs
WHERE FALSE;

ROLLBACK;
