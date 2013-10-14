-- Deploy jobs
-- requires: job_status

BEGIN;

CREATE TABLE IF NOT EXISTS jobs (
    id character(32) NOT NULL PRIMARY KEY,
    org_id character(32) NOT NULL,
    command text NOT NULL,
    status integer NOT NULL REFERENCES job_status(id),
    run_timeout integer,
    last_updated_by character(32) NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    quorum integer
);

COMMIT;
