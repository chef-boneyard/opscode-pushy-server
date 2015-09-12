# -*- indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
#
# Author:: Steven Grady (<steven.grady@erlang-solutions.com>)

# @copyright Copyright 2014 Chef Software, Inc. All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License. You may obtain
# a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

require 'pushy/spec_helper'
require 'fileutils'

describe "filedir-test" do
  include_context "end_to_end_util"
  context "when a client is started that has a 2 second expiration time on the filedir" do
    let(:file_dir_expiry_limit) {10}

    before :each do
      start_new_clients(['DONKEY'], {:file_dir_expiry => 2})
    end

    after :each do
      if @clients
        @clients.each do |client_name, client|
          stop_client(client_name) if @clients[client_name][:client]
        end
        @clients = nil
      end
    end

    it 'within #{file_dir_expiry_limit} seconds, cleans up a newly-created file' do
      prep_tmp_path
      filestr = 'raw:test'
      job = start_job('ruby-opts', ['DONKEY'], {'file' => filestr})
      wait_for_job_complete(job['uri'])
      envs = read_tmp_path[3]
      env = Hash[eval envs]
      path = env['CHEF_PUSH_JOB_FILE']
      File::exists?(path).should == true
      sleep file_dir_expiry_limit
      File::exists?(path).should == false
    end

    it "non-pushy files won't be touched" do
      file_dir = @clients['DONKEY'][:client].file_dir
      file_path = file_dir + "/not_a_pushy_file"
      File.open(file_path, "w+") {|f| f.write("keep me")}
      prep_tmp_path
      filestr = 'raw:test'
      job = start_job('ruby-opts', ['DONKEY'], {'file' => filestr})
      wait_for_job_complete(job['uri'])
      sleep 4
      File::exists?(file_path).should == true
    end
  end
end
