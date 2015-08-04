-- @copyright Copyright 2015 Chef Software, Inc. All Rights Reserved.
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

CREATE OR REPLACE FUNCTION test_job_output_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('job_output');

  -- Columns
  RETURN QUERY SELECT col_type_is('job_output', 'id', 'integer');
  RETURN QUERY SELECT col_not_null('job_output', 'id');

  RETURN QUERY SELECT col_type_is('job_output', 'node_name', 'text');
  RETURN QUERY SELECT col_not_null('job_output', 'node_name');

  RETURN QUERY SELECT col_type_is('job_output', 'stdout', 'text');
  RETURN QUERY SELECT col_not_null('job_output', 'stdout');

  RETURN QUERY SELECT col_type_is('job_output', 'stderr', 'text');
  RETURN QUERY SELECT col_not_null('job_output', 'stderr');

  RETURN QUERY SELECT col_is_pk('job_output', 'id');

  -- Keys
  RETURN QUERY SELECT has_pk('job_output');
  RETURN QUERY SELECT hasnt_fk('job_output');

END;
$$;
