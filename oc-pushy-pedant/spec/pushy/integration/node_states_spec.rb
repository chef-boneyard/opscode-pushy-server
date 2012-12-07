#
# -*- indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
# ex: ts=4 sw=4 et
#
# Author:: Douglas Triggs (<doug@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#

require 'pedant/rspec/common'
require 'pushy/support/authorization_groups_util'

describe "Node_States API Endpoint", :node_states do
  include_context "authorization_groups_util"

  let(:node_name) { 'some_node' }
  let(:non_existent_node_name) { 'not_a_number' }

  let(:failed_to_authenticate_as_invalid_msg) {
    ["Failed to authenticate as 'invalid'. Ensure that your node_name and client key are correct."] }
  let(:non_member_authorization_failed_msg) {
    ["User or client 'pedant_admin_user' does not have access to that action on this server."] }
  let(:non_member_client_authorization_failed_msg) {
    ["User or client 'pedant_admin_client' does not have access to that action on this server."] }
  let(:outside_user_not_associated_msg) {
    ["'pedant-nobody' not associated with organization '#{org}'"] }
  let(:cannot_load_nonexistent_msg) { 
    ["Cannot load client #{non_existent_node_name}"] }
  let(:node_state_status) {
    {
      "availability" => "unavailable",
      "node_name" => node_name,
      "status" => "offline"
    } }

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

      it 'returns a 200 ("OK") for admin client' do
        get(api_url("/pushy/node_states/"), platform.admin_client) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 200 ("OK") for non-admin client', :pending do
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

      it 'returns a 403 ("Forbidden") for outside user', :pending do
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

      it 'returns a 200 ("OK") for non-admin client', :pending do
        get(api_url("/pushy/node_states/#{node_name}"),
            platform.non_admin_client) do |response|
          response.should look_like({
                                      :status => 200,
                                      :body_exact => node_state_status
                                    })
        end
      end

      it 'returns a 200 ("OK") for admin client' do
        get(api_url("/pushy/node_states/#{node_name}"),
            platform.admin_client) do |response|
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

      it 'returns a 403 ("Forbidden") for outside user', :pending do
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

      it 'returns a 404 ("Not Found") for missing node_state for admin', :pending do
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

      it 'returns a 404 ("Not Found") for missing node_state for normal user', :pending do

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
      setup_group("pushy_job_readers", [member.name], [member_client.name], [])
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

      it 'returns a 200 ("OK") for member client', :pending do
        # TODO: same authn problem as above
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

      it 'returns a 200 ("OK") for member client', :pending do
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
  end # describe 'access control with pushy_job_readers'

  describe 'access control with pushy_job_readers and nested groups' do
    # Doing these in reverse for extra fun; this will guarantee it doesn't
    # "accidentally" work if the groups are missing
    let(:member) { normal_user }
    let(:non_member) { admin_user }
    let(:member_client) { platform.non_admin_client }
    let(:non_member_client) { platform.admin_client }

    before(:all) do
      setup_group("nested_pushy_job_readers", [member.name], [member_client.name], [])
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

      it 'returns a 200 ("OK") for member client', :pending do
        # TODO: same authn problem as above
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

      it 'returns a 200 ("OK") for member client', :pending do
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
end
