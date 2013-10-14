-- Deploy job_nodes
-- requires: jobs

BEGIN;

CREATE TABLE IF NOT EXISTS job_nodes (
    job_id character(32) REFERENCES jobs(id) ON UPDATE CASCADE ON DELETE CASCADE,
    org_id character(32) NOT NULL,
    node_name text NOT NULL,

    UNIQUE(job_id, org_id, node_name),

    status integer NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL
);

COMMIT;
