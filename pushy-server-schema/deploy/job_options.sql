-- Deploy job_options
-- requires: jobs

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

BEGIN;

CREATE TABLE IF NOT EXISTS job_options (
    job_id character(32) NOT NULL PRIMARY KEY
           REFERENCES jobs(id) ON UPDATE CASCADE ON DELETE CASCADE,
    job_user character(32), -- user to run job as
    dir text,               -- directory to execute command in
    env text,               -- environment variables to set
    capture boolean,        -- ???
    job_file text           -- file to send to node
);

COMMIT;
