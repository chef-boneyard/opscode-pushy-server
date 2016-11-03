-- Deploy job_nodes
-- requires: jobs

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
