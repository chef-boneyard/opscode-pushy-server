CREATE OR REPLACE FUNCTION test_job_status_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('job_status');

  -- Columns
  RETURN QUERY SELECT col_type_is('job_status', 'id', 'integer');
  RETURN QUERY SELECT col_not_null('job_status', 'id');

  RETURN QUERY SELECT col_type_is('job_status', 'description', 'text');
  RETURN QUERY SELECT col_not_null('job_status', 'description');

  RETURN QUERY SELECT col_is_pk('job_status', 'id');

  -- Keys
  RETURN QUERY SELECT has_pk('job_status');
  RETURN QUERY SELECT hasnt_fk('job_status');

  -- Values
  RETURN QUERY SELECT results_eq(
    'SELECT id, description FROM job_status ORDER BY id',
    'VALUES (0, ''voting''),
            (1, ''running''),
            (2, ''complete''),
            (3, ''quorum_failed''),
            (4, ''aborted''),
            (5, ''new''),
            (6, ''timed_out''),
            (7, ''crashed'')',
    'All expected values are present'
  );

END;
$$;
