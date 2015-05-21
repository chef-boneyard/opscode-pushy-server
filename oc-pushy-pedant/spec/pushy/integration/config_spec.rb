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

require 'pedant/rspec/auth_headers_util'
require 'pushy/support/authorization_groups_util'

describe "pushy config" do
  include_context "authorization_groups_util"

  def self.ruby?
    false
  end

  let(:pushy_server) {
    Pedant.config[:pushy_server]
  }

  # TODO: should get this from config instead of hard-coding
  let(:config_name) { 'DONKEY' }

  let(:failed_to_authenticate_as_invalid_msg) {
    ["Failed to authenticate as 'invalid'. Ensure that your node_name and client key are correct."] }
  let(:outside_user_not_associated_msg) {
    ["'pedant-nobody' is not associated with organization '#{org}'"] }
  let(:client_and_node_name_match_msg) {
    ["Client and node name must match"] }
  let(:non_member_authorization_failed_msg) {
    ["User or client 'pedant_admin_user' does not have access to that action on this server."] }
  let(:non_member_client_authorization_failed_msg) {
    ["User or client 'pedant_admin_client' does not have access to that action on this server."] }

  let(:config_body) {
    {
      # We don't bother testing a number of values here (since they're somewhat
      # arbitrary and change from run to run in most cases); we just want to confirm
      # that they exist.
      "type" => "config",
      "host" => pushy_server,
      "node" => config_name,
      "organization" => /^pedant-testorg-.*/,
      "public_key" => /^-----BEGIN PUBLIC KEY-----/,
      "push_jobs" => {
        "heartbeat" => {
          "out_addr" => "tcp://#{pushy_server}:10000",
          "command_addr" => "tcp://#{pushy_server}:10002",
          "interval" => 1.0,
          "offline_threshold" => 3,
          "online_threshold" => 2
        }
      },
      "encoded_session_key" => {
        "method" => "hmac_sha256",
        "key" => /.*/
      },
      "lifetime" => 3600,
      "max_message_skew" => 300,
      "incarnation_id" => /.*/,
    } }

  context "/organization/<name>/pushy/config/" do
    it "GET returns 404" do
      get(api_url("pushy/config/"), admin_user) do |response|
        response.should look_like({
            :status => 404
          })
      end
    end

    it "PUT returns 404" do
      put(api_url("pushy/config/"), admin_user, :payload => config_body) do |response|
        response.should look_like({
            :status => 404
          })
      end
    end

    it "POST returns 404" do
      post(api_url("pushy/config/"), admin_user, :payload => config_body) do |response|
        response.should look_like({
            :status => 404
          })
      end
    end

    it "DELETE returns 404" do
      delete(api_url("pushy/config/"), admin_user) do |response|
        response.should look_like({
            :status => 404
          })
      end
    end
  end # context "/organization/<name>/pushy/config/"

  context "GET /organization/<name>/pushy/config/<nodename>" do
    it "returns 200 and server config" do
      get(api_url("pushy/config/DONKEY"), admin_user) do |response|
        # TODO: probably should grab the correct values from config instead of
        # hard-coding this, but this is proof-of-concept test only right now
        response.should look_like({
            :status => 200,
            :body => config_body
          })
      end
    end
  end # context "GET /organization/<name>/<pushy>/node_states"

  context "PUT /organization/<name>/pushy/config/<nodename>" do
    it "returns 405" do
      put(api_url("pushy/config/DONKEY"), admin_user,
        :payload => config_body) do |response|
        response.should look_like({
            :status => 405
          })
      end
    end
  end # context "PUT /organization/<name>/<pushy>/node_states"

  context "POST /organization/<name>/pushy/config/<nodename>" do
    it "returns 405" do
      post(api_url("pushy/config/DONKEY"), admin_user,
        :payload => config_body) do |response|
        response.should look_like({
            :status => 405
          })
      end
    end
  end # context "POST /organization/<name>/<pushy>/node_states"

  context "DELETE /organization/<name>/pushy/config/<nodename>" do
    it "returns 405" do
      delete(api_url("pushy/config/DONKEY"), admin_user) do |response|
        response.should look_like({
            :status => 405
          })
      end
    end
  end # context "DELETE /organization/<name>/<pushy>/node_states"

  describe 'access control' do
    context 'GET /config/<name>' do
      it 'returns a 200 ("OK") for admin' do
        get(api_url("/pushy/config/#{config_name}"), admin_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 200 ("OK") for normal user' do
        get(api_url("/pushy/config/#{config_name}"), normal_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 200 ("OK") for client' do
        get(api_url("/pushy/config/pedant_non_admin_client"),
            platform.non_admin_client) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 401 ("Unauthorized") for invalid user' do
        get(api_url("/pushy/config/#{config_name}"),
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
        get(api_url("/pushy/config/#{config_name}"),
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
        get(api_url("/pushy/config/#{config_name}"),
            platform.bad_client) do |response|
          response.should look_like({
                                      :status => 401
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") when client and node name do not match' do
        get(api_url("/pushy/config/#{config_name}"),
            platform.non_admin_client) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => client_and_node_name_match_msg
                                      }
                                    })
        end
      end
    end
  end # describe 'access control'
  
  describe 'access control with pushy_job groups' do
    # Doing these in reverse for extra fun; this will guarantee it doesn't
    # "accidentally" work if the groups are missing
    let(:member) { normal_user }
    let(:non_member) { admin_user }
    let(:member_client) { platform.non_admin_client }
    let(:non_member_client) { platform.admin_client }

    before(:all) do
      setup_group("pushy_job_readers", [normal_user.name, outside_user.name],
                  [platform.non_admin_client.name], [])
      setup_group("pushy_job_writers", [normal_user.name, outside_user.name],
                  [platform.non_admin_client.name], [])
    end

    after(:all) do
      delete(api_url("/groups/pushy_job_readers"), superuser)
      delete(api_url("/groups/pushy_job_writers"), superuser)
    end

    context 'GET /config/<name> with pushy_job_readers' do
      # So.  Something is wrong here, but I'm not entirely clear on what.  I suspect
      # that the fact that these first two tests succeed is actually an issue and
      # the client tests aren't: my understanding is that the clients should ignore
      # the groups (apparently they do?) and that users should as well (apparently
      # they don't?).  So one way or the other, we apparently have an auth issue
      # here which probably needs to be addressed at some point.

      it 'returns a 200 ("OK") for member' do
        get(api_url("/pushy/config/#{config_name}"), member) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member' do
        get(api_url("/pushy/config/#{config_name}"), non_member) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => non_member_authorization_failed_msg
                                      }
                                    })
        end
      end

      it 'returns a 200 ("OK") for member client' do
        pending 'something weird going on for clients here' do
          get(api_url("/pushy/config/#{config_name}"), member_client) do |response|
            response.should look_like({
                :status => 200
              })
          end
        end
      end

      it 'returns a 403 ("Forbidden") for non-member client' do
        pending 'something weird going on for clients here' do
          get(api_url("/pushy/config/#{config_name}"), non_member_client) do |response|
            response.
              should look_like({
                :status => 403,
                :body_exact => {
                  "error" => non_member_client_authorization_failed_msg
                }
              })
          end
        end
      end

      it 'returns a 403 ("Forbidden") for outside user' do
        get(api_url("/pushy/config/#{config_name}"),
            outside_user) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => outside_user_not_associated_msg
                                      }
                                    })
        end
      end
    end # context 'GET /config/<name> with pushy_job_readers'
  end # describe 'access control with pushy_job groups'

  context 'invalid request' do
    it "returns 404 (\"Not Found\") when organization doesn't exist" do
      # This should be un-pended when OC-5484 is done
      path = api_url("/pushy/config/#{config_name}").gsub(org, "bogus-org")
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

    context 'GET /config/<name>' do
      let(:url) { api_url("/pushy/config/#{config_name}") }
      let(:response_should_be_successful) do
        response.should look_like({
                                    :status => 200,
                                    :body => config_body
                                  })
      end

      include_context 'handles authentication headers correctly'
    end
  end
end # describe "pushy config"
