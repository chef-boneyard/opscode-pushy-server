-- @copyright Copyright 2014 Chef Software, Inc. All Rights Reserved.
--
-- This file is provided to you under the Apache License,
-- Version 2.0 (the "License"); you may not use this file
-- except in compliance with the License. You may obtain
-- a copy of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing,
-- software distributed under the License is distributed on an
-- "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
-- KIND, either express or implied. See the License for the
-- specific language governing permissions and limitations
-- under the License.
--

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
