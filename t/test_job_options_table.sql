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

CREATE OR REPLACE FUNCTION test_job_options_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('job_options');

  -- Columns
  RETURN QUERY SELECT col_type_is('job_options', 'id', 'integer');
  RETURN QUERY SELECT col_not_null('job_options', 'id');

  RETURN QUERY SELECT col_type_is('job_options', 'job_user', 'text');
  RETURN QUERY SELECT col_not_null('job_options', 'job_user');

  RETURN QUERY SELECT col_type_is('job_options', 'env', 'text');
  RETURN QUERY SELECT col_not_null('job_options', 'env');

  RETURN QUERY SELECT col_type_is('job_options', 'capture', 'boolean');
  RETURN QUERY SELECT col_not_null('job_options', 'capture');

  RETURN QUERY SELECT col_type_is('job_options', 'env', 'text');
  RETURN QUERY SELECT col_not_null('job_options', 'env');

  RETURN QUERY SELECT col_is_pk('job_options', 'id');

  -- Keys
  RETURN QUERY SELECT has_pk('job_options');
  RETURN QUERY SELECT hasnt_fk('job_options');

END;
$$;
