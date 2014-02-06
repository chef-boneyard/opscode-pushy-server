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

CREATE OR REPLACE FUNCTION test_job_nodes_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN
  RETURN QUERY SELECT has_table('job_nodes');

  -- TODO: THIS COLUMN REALLY SHOULD BE NOT NULL!
  --  RETURN QUERY SELECT chef_pgtap.col_is_uuid('job_nodes', 'job_id');
  RETURN QUERY SELECT has_column('job_nodes', 'job_id');
  RETURN QUERY SELECT col_is_null('job_nodes', 'job_id');  -- TODO: THIS IS WRONG!
  RETURN QUERY SELECT col_type_is('job_nodes', 'job_id', 'character(32)');
  RETURN QUERY SELECT col_hasnt_default('job_nodes', 'job_id');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('job_nodes', 'org_id');
  RETURN QUERY SELECT chef_pgtap.col_is_name('job_nodes', 'node_name');

  -- TODO: This should be the PK
  RETURN QUERY SELECT col_is_unique('job_nodes', ARRAY['job_id', 'org_id', 'node_name']);

  -- TODO: This should be a new enum distinct from the job_status; see
  -- https://github.com/opscode/opscode-pushy-server/blob/master/apps/pushy/include/pushy_sql.hrl#L10-L30
  RETURN QUERY SELECT has_column('job_nodes', 'status');
  RETURN QUERY SELECT col_not_null('job_nodes','status');
  RETURN QUERY SELECT col_type_is('job_nodes', 'status', 'integer');

  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('job_nodes', 'created_at');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('job_nodes', 'updated_at');

  -- TODO: DANGER, WILL ROBINSON!!
  RETURN QUERY SELECT hasnt_pk('job_nodes');

  -- TODO: For some reason, this test fails, even though we very clearly have foreign keys
  -- RETURN QUERY SELECT has_fk('job_nodes');

  RETURN QUERY SELECT fk_ok('job_nodes', 'job_id',
                            'jobs', 'id');

  RETURN QUERY SELECT chef_pgtap.fk_update_action_is('job_nodes_job_id_fkey', 'CASCADE');
  RETURN QUERY SELECT chef_pgtap.fk_delete_action_is('job_nodes_job_id_fkey', 'CASCADE');

END;
$$;
