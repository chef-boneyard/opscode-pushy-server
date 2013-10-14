-- Verify job_status

BEGIN;

SELECT id, description
FROM job_status
WHERE FALSE;

ROLLBACK;
