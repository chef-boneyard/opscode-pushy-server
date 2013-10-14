-- Deploy job_status

BEGIN;

CREATE TABLE job_status (
    id integer NOT NULL PRIMARY KEY,
    description text NOT NULL
);

INSERT INTO job_status VALUES
  (0, 'voting'),
  (1, 'running'),
  (2, 'complete'),
  (3, 'quorum_failed'),
  (4, 'aborted'),
  (5, 'new'),
  (6, 'timed_out'),
  (7, 'crashed');

COMMIT;
