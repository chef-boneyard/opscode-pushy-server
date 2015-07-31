-- Verify job_options

BEGIN;

-- XXX Add verifications here.
SELECT job_id, job_user, dir, env, capture, job_file FROM job_options
WHERE FALSE;


ROLLBACK;
