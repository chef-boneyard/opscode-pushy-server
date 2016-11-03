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

CREATE OR REPLACE FUNCTION test_jobs_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN
   RETURN QUERY SELECT has_table('jobs');

   RETURN QUERY SELECT chef_pgtap.col_is_uuid('jobs', 'id');
   RETURN QUERY SELECT col_is_pk('jobs', 'id');

   RETURN QUERY SELECT chef_pgtap.col_is_uuid('jobs', 'org_id');

   RETURN QUERY SELECT has_column('jobs', 'command');
   RETURN QUERY SELECT col_not_null('jobs', 'command');
   RETURN QUERY SELECT col_type_is('jobs','command', 'text');
   RETURN QUERY SELECT col_hasnt_default('jobs', 'command');

   RETURN QUERY SELECT has_column('jobs', 'status');
   RETURN QUERY SELECT col_not_null('jobs','status');
   -- TODO: This should really be an enum type
   RETURN QUERY SELECT col_type_is('jobs', 'status', 'integer');

   RETURN QUERY SELECT has_column('jobs', 'run_timeout');
   RETURN QUERY SELECT col_is_null('jobs','run_timeout'); -- TODO: really?
   RETURN QUERY SELECT col_type_is('jobs', 'run_timeout', 'integer');
   RETURN QUERY SELECT col_hasnt_default('jobs', 'run_timeout'); -- TODO: really?

   RETURN QUERY SELECT chef_pgtap.col_is_uuid('jobs','last_updated_by');

   RETURN QUERY SELECT chef_pgtap.col_is_timestamp('jobs', 'created_at');
   RETURN QUERY SELECT chef_pgtap.col_is_timestamp('jobs', 'updated_at');

   RETURN QUERY SELECT has_column('jobs', 'quorum');
   RETURN QUERY SELECT col_is_null('jobs','quorum'); -- TODO: really?
   RETURN QUERY SELECT col_type_is('jobs', 'quorum', 'integer');
   RETURN QUERY SELECT col_hasnt_default('jobs', 'quorum'); -- TODO: really?

   RETURN QUERY SELECT fk_ok('jobs', 'status',
                             'job_status', 'id');
END;
$$;
