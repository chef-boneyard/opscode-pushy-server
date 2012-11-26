#
# -*- indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
# ex: ts=4 sw=4 et
#
# Author:: Douglas Triggs (<doug@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#

require 'pedant/rspec/common'

describe "Node_States API Endpoint", :node_states do
  let(:node_name) { 'some_node' }
  let(:non_existent_node_name) { 'not_a_number' }

  let(:failed_to_authenticate_as_invalid_msg) {
    ["Failed to authenticate as 'invalid'. Ensure that your node_name and client key are correct."] }
  let(:outside_user_not_associated_msg) {
    ["'pedant-nobody' not associated with organization '#{org}'"] }
  let(:cannot_load_nonexistent_msg) { 
    ["Cannot load client #{non_existent_node_name}"] }
  let(:payload) {
    {
      "availability" => "unavailable",
      "node_name" => node_name,
      "status" => "offline"
    } }

  describe 'access control' do
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
        get(api_url("/pushy/node_states/"), client) do |response|
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
    end

    context 'GET /node_states/<name>' do
      it 'returns a 200 ("OK") for admin' do
        get(api_url("/pushy/node_states/#{node_name}"), admin_user) do |response|
          response.should look_like({
                                      :status => 200,
                                      :body_exact => payload
                                    })
        end
      end

      it 'returns a 200 ("OK") for normal user' do
        get(api_url("/pushy/node_states/#{node_name}"), normal_user) do |response|
          response.should look_like({
                                      :status => 200,
                                      :body_exact => payload
                                    })
        end
      end

      it 'returns a 200 ("OK") for client' do
        get(api_url("/pushy/node_states/#{node_name}"), client) do |response|
          response.should look_like({
                                      :status => 200,
                                      :body_exact => payload
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
    end
  end
end
