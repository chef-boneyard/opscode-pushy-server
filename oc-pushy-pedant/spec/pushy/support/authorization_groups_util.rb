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

shared_context "authorization_groups_util" do
  def setup_group(group_name, users, clients, subgroups)
    post(api_url("/groups"), superuser,
         :payload => { "groupname" => group_name }) do |response|
      response.should look_like({
                                  :status => 201
                                })

    end
    put(api_url("/groups/#{group_name}"), superuser,
        :payload => { "groupname" => group_name, "actors" => { "users" => users,
            "clients" => clients, "groups" => subgroups } } ) do |response|
      response.should look_like({
                                  :status => 200
                                })
    end
  end
end
