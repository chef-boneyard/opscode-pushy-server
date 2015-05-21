#
# -*- indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
# ex: ts=4 sw=4 et
#
# Author:: Douglas Triggs (<doug@opscode.com>)
#

# @copyright Copyright 2012 Chef Software, Inc. All Rights Reserved.
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

require 'pedant/rspec/common'
require 'pedant/rspec/auth_headers_util'
require 'pushy/support/authorization_groups_util'

describe "Node_States API Endpoint", :node_states do
  include_context "authorization_groups_util"

  def self.ruby?
    false
  end

  let(:node_name) { 'some_node' }
  let(:non_existent_node_name) { 'not_a_number' }

  let(:failed_to_authenticate_as_invalid_msg) {
    ["Failed to authenticate as 'invalid'. Ensure that your node_name and client key are correct."] }
  let(:non_member_authorization_failed_msg) {
    ["User or client 'pedant_admin_user' does not have access to that action on this server."] }
  let(:non_member_client_authorization_failed_msg) {
    ["User or client 'pedant_admin_client' does not have access to that action on this server."] }
  let(:outside_user_not_associated_msg) {
    ["'pedant-nobody' is not associated with organization '#{org}'"] }
  let(:cannot_load_nonexistent_msg) {
    ["Cannot load client #{non_existent_node_name}"] }
  let(:node_state_status) {
    {
      "availability" => "unavailable",
      "node_name" => node_name,
      "status" => "offline"
    } }

  describe 'HTTP verb validation' do
    context '/organizations/<org>/pushy/node_states/' do
      it 'PUT returns a 405 ("Method Not Allowed")' do
        put(api_url("/pushy/node_states/"), admin_user,
          :payload => node_state_status) do |response|
          response.should look_like({
              :status => 405
            })
        end
      end

      it 'POST returns a 405 ("Method Not Allowed")' do
        post(api_url("/pushy/node_states/"), admin_user,
          :payload => node_state_status) do |response|
          response.should look_like({
              :status => 405
            })
        end
      end

      it 'DELETE returns a 405 ("Method Not Allowed")' do
        delete(api_url("/pushy/node_states/"), admin_user) do |response|
          response.should look_like({
              :status => 405
            })
        end
      end
    end # context '/organizations/<org>/pushy/jobs/'

    context '/organizations/<org>/pushy/node_states/<nodename>' do
      it 'PUT returns a 405 ("Method Not Allowed")' do
        put(api_url("/pushy/node_states/#{node_name}"), admin_user,
          :payload => node_state_status) do |response|
          response.should look_like({
              :status => 405
            })
        end
      end

      it 'POST returns a 405 ("Method Not Allowed")' do
        post(api_url("/pushy/node_states/#{node_name}"), admin_user,
          :payload => node_state_status) do |response|
          response.should look_like({
              :status => 405
            })
        end
      end

      it 'DELETE returns a 405 ("Method Not Allowed")' do
        delete(api_url("/pushy/node_states/#{node_name}"), admin_user) do |response|
          response.should look_like({
              :status => 405
            })
        end
      end
    end # context '/organizations/<org>/pushy/jobs/<name>'
  end # describe 'HTTP verb validation'

  describe 'access control with no pushy_job_readers' do
    context 'GET /node_states' do
      it 'returns a 200 ("OK") for admin' do
        get(api_url("/pushy/node_states/"), admin_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 200 ("OK") for normal user' do
        get(api_url("/pushy/node_states/"), normal_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 200 ("OK") for client' do
        get(api_url("/pushy/node_states/"), platform.non_admin_client) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 401 ("Unauthorized") for invalid user' do
        get(api_url("/pushy/node_states"),
            invalid_user) do |response|
          response.
            should look_like({
                               :status => 401,
                               :body_exact => {
                                 "error" => failed_to_authenticate_as_invalid_msg
                               }
                             })
        end
      end

      it 'returns a 403 ("Forbidden") for outside user' do
        get(api_url("/pushy/node_states"),
            outside_user) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => outside_user_not_associated_msg
                                      }
                                    })
        end
      end

      it 'returns a 401 ("Unauthorized") for bogus client' do
        get(api_url("/pushy/node_states"),
            platform.bad_client) do |response|
          response.should look_like({
                                      :status => 401
                                    })
        end
      end
    end # context 'GET /node_states'

    context 'GET /node_states/<name>' do
      it 'returns a 200 ("OK") for admin' do
        get(api_url("/pushy/node_states/#{node_name}"), admin_user) do |response|
          response.should look_like({
                                      :status => 200,
                                      :body_exact => node_state_status
                                    })
        end
      end

      it 'returns a 200 ("OK") for normal user' do
        get(api_url("/pushy/node_states/#{node_name}"), normal_user) do |response|
          response.should look_like({
                                      :status => 200,
                                      :body_exact => node_state_status
                                    })
        end
      end

      it 'returns a 200 ("OK") for client' do
        get(api_url("/pushy/node_states/#{node_name}"),
            platform.non_admin_client) do |response|
          response.should look_like({
                                      :status => 200,
                                      :body_exact => node_state_status
                                    })
        end
      end

      it 'returns a 401 ("Unauthorized") for invalid user' do
        get(api_url("/pushy/node_states/#{node_name}"),
            invalid_user) do |response|
          response.should look_like({
                                      :status => 401,
                                      :body_exact => {
                                        "error" => failed_to_authenticate_as_invalid_msg
                                      }
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for outside user' do
        get(api_url("/pushy/node_states/#{node_name}"),
            outside_user) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => outside_user_not_associated_msg
                                      }
                                    })
        end
      end

      it 'returns a 401 ("Unauthorized") for bogus client' do
        get(api_url("/pushy/node_states/#{node_name}"),
            platform.bad_client) do |response|
          response.should look_like({
                                      :status => 401
                                    })
        end
      end

      it 'returns a 404 ("Not Found") for missing node_state for admin', :skip do
        get(api_url("/pushy/node_states/#{non_existent_node_name}"),
            admin_user) do |response|
          response.should look_like({
                                      :status => 404,
                                      :body_exact => {
                                        "error" => cannot_load_nonexistent_msg
                                      }
                                    })
        end
      end

      it 'returns a 404 ("Not Found") for missing node_state for normal user', :skip do

        get(api_url("/pushy/node_states/#{non_existent_node_name}"),
            normal_user) do |response|
          response.should look_like({
                                      :status => 404,
                                      :body_exact => {
                                        "error" => cannot_load_nonexistent_msg
                                      }
                                    })
        end
      end
    end # context 'GET /node_states/<name>'
  end # describe 'access control with no pushy_job_readers'

  describe 'access control with pushy_job_readers' do
    # Doing these in reverse for extra fun; this will guarantee it doesn't
    # "accidentally" work if the groups are missing
    let(:member) { normal_user }
    let(:non_member) { admin_user }
    let(:member_client) { platform.non_admin_client }
    let(:non_member_client) { platform.admin_client }

    before(:all) do
      @member = normal_user
      @member_client = platform.non_admin_client

      setup_group("pushy_job_readers", [@member.name, outside_user.name],
                  [@member_client.name], [])
    end

    after(:all) do
      delete(api_url("/groups/pushy_job_readers"), superuser)
    end

    context 'GET /node_states' do
      it 'returns a 200 ("OK") for member' do
        get(api_url("/pushy/node_states/"), member) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member' do
        get(api_url("/pushy/node_states/"), non_member) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => non_member_authorization_failed_msg
                                      }
                                    })
        end
      end

      it 'returns a 200 ("OK") for member client' do
        get(api_url("/pushy/node_states/"), member_client) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member client' do
        get(api_url("/pushy/node_states/"), non_member_client) do |response|
          response.
            should look_like({
                               :status => 403,
                               :body_exact => {
                                 "error" => non_member_client_authorization_failed_msg
                               }
                             })
        end
      end

      it 'returns a 403 ("Forbidden") for outside user' do
        get(api_url("/pushy/node_states"),
            outside_user) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => outside_user_not_associated_msg
                                      }
                                    })
        end
      end
    end # context 'GET /node_states'

    context 'GET /node_states/<name>' do
      it 'returns a 200 ("OK") for member' do
        get(api_url("/pushy/node_states/#{node_name}"), member) do |response|
          response.should look_like({
                                      :status => 200,
                                      :body_exact => node_state_status
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member' do
        get(api_url("/pushy/node_states/#{node_name}"), non_member) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => non_member_authorization_failed_msg
                                      }
                                    })
        end
      end

      it 'returns a 200 ("OK") for member client' do
        get(api_url("/pushy/node_states/#{node_name}"),
            member_client) do |response|
          response.should look_like({
                                      :status => 200,
                                      :body_exact => node_state_status
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member client' do
        get(api_url("/pushy/node_states/#{node_name}"),
            non_member_client) do |response|
          response.
            should look_like({
                               :status => 403,
                               :body_exact => {
                                 "error" => non_member_client_authorization_failed_msg
                               }
                             })
        end
      end

      it 'returns a 403 ("Forbidden") for outside user' do
        get(api_url("/pushy/node_states/#{node_name}"),
            outside_user) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => outside_user_not_associated_msg
                                      }
                                    })
        end
      end
    end # context 'GET /node_states/<name>'
  end # describe 'access control with pushy_job_readers'

  describe 'access control with pushy_job_readers and nested groups' do
    # Doing these in reverse for extra fun; this will guarantee it doesn't
    # "accidentally" work if the groups are missing
    let(:member) { normal_user }
    let(:non_member) { admin_user }
    let(:member_client) { platform.non_admin_client }
    let(:non_member_client) { platform.admin_client }

    before(:all) do
      @member = normal_user
      @member_client = platform.non_admin_client

      setup_group("nested_pushy_job_readers", [@member.name], [@member_client.name], [])
      setup_group("pushy_job_readers", [], [], ["nested_pushy_job_readers"])
    end

    after(:all) do
      delete(api_url("/groups/pushy_job_readers"), superuser)
      delete(api_url("/groups/nested_pushy_job_readers"), superuser)
    end

    context 'GET /node_states' do
      it 'returns a 200 ("OK") for member' do
        get(api_url("/pushy/node_states/"), member) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member' do
        get(api_url("/pushy/node_states/"), non_member) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => non_member_authorization_failed_msg
                                      }
                                    })
        end
      end

      it 'returns a 200 ("OK") for member client' do
        get(api_url("/pushy/node_states/"), member_client) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member client' do
        get(api_url("/pushy/node_states/"), non_member_client) do |response|
          response.
            should look_like({
                               :status => 403,
                               :body_exact => {
                                 "error" => non_member_client_authorization_failed_msg
                               }
                             })
        end
      end
    end # context 'GET /node_states'

    context 'GET /node_states/<name>' do
      it 'returns a 200 ("OK") for member' do
        get(api_url("/pushy/node_states/#{node_name}"), member) do |response|
          response.should look_like({
                                      :status => 200,
                                      :body_exact => node_state_status
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member' do
        get(api_url("/pushy/node_states/#{node_name}"), non_member) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => non_member_authorization_failed_msg
                                      }
                                    })
        end
      end

      it 'returns a 200 ("OK") for member client' do
        get(api_url("/pushy/node_states/#{node_name}"),
            member_client) do |response|
          response.should look_like({
                                      :status => 200,
                                      :body_exact => node_state_status
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member client' do
        get(api_url("/pushy/node_states/#{node_name}"),
            non_member_client) do |response|
          response.
            should look_like({
                               :status => 403,
                               :body_exact => {
                                 "error" => non_member_client_authorization_failed_msg
                               }
                             })
        end
      end
    end # context 'GET /node_states/<name>'
  end # describe 'access control with pushy_job_readers and nested groups'

  context 'invalid request' do
    it 'returns 404 ("Not Found") with bogus org for /node_states' do
      path = api_url("/pushy/node_states").gsub(org, "bogus-org")
      get(path, admin_user) do |response|
        response.should look_like({
                                    :status => 404
                                  })
      end
    end

    it 'returns 404 ("Not Found") with bogus org for /node_states/<name>' do
      path = api_url("/pushy/node_states/#{node_name}").gsub(org, "bogus-org")
      get(path, admin_user) do |response|
        response.should look_like({
                                    :status => 404
                                  })
      end
    end
  end

  describe 'handling authentication headers' do
    let(:method) { :GET }
    let(:body) { nil }
    let(:success_user) { admin_user }
    let(:failure_user) { invalid_user }

    context 'GET /node_states/' do
      let(:url) { api_url("/pushy/node_states") }
      let(:response_should_be_successful) do
        response.should look_like({
                                    :status => 200
                                    # TODO: seems it's still not matching arrays
                                    # correctly; Didn't John fix this at some point?
                                  })
      end

      include_context 'handles authentication headers correctly'
    end

    context 'GET /node_states/<name>' do
      let(:url) { api_url("/pushy/node_states/#{node_name}") }
      let(:response_should_be_successful) do
        response.should look_like({
                                    :status => 200,
                                    :body_exact => node_state_status
                                  })
      end

      include_context 'handles authentication headers correctly'
    end
  end
end
