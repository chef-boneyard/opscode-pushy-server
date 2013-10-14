-- Verify job_nodes

BEGIN;

SELECT job_id, org_id, node_name, status, created_at, updated_at
FROM job_nodes
WHERE FALSE;

ROLLBACK;
