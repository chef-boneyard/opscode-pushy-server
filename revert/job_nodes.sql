-- Revert job_nodes

BEGIN;

DROP TABLE IF EXISTS job_nodes;

COMMIT;
