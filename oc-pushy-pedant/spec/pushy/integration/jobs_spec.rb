#
# -*- indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
# ex: ts=4 sw=4 et
#
# Author:: Douglas Triggs (<doug@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#

require 'pedant/rspec/common'

describe "Jobs API Endpoint", :jobs do
  let(:job_name) {
    get(api_url("/pushy/jobs"), admin_user) do |response|
      list = JSON.parse(response.body)
      list[0]["id"]
    end
  }
  let(:non_existent_job) { 'not_a_number' }

  let(:failed_to_authenticate_as_invalid_msg) {
    ["Failed to authenticate as 'invalid'. Ensure that your node_name and client key are correct."] }
  let(:outside_user_not_associated_msg) {
    ["'pedant-nobody' not associated with organization '#{org}'"] }

  describe 'access control' do
    context 'GET /jobs' do
      it 'returns a 200 ("OK") for admin' do
        get(api_url("/pushy/jobs/"), admin_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 200 ("OK") for normal user' do
        get(api_url("/pushy/jobs/"), normal_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 200 ("OK") for client' do
        get(api_url("/pushy/jobs/"), client) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 401 ("Unauthorized") for invalid user' do
        get(api_url("/pushy/jobs"),
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
        get(api_url("/pushy/jobs"),
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

    context 'GET /jobs/<name>' do
      it 'returns a 200 ("OK") for admin' do
        get(api_url("/pushy/jobs/#{job_name}"), admin_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 200 ("OK") for normal user' do
        get(api_url("/pushy/jobs/#{job_name}"), normal_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 200 ("OK") for client' do
        get(api_url("/pushy/jobs/#{job_name}"), client) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 401 ("Unauthorized") for invalid user' do
        get(api_url("/pushy/jobs/#{job_name}"),
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
        get(api_url("/pushy/jobs/#{job_name}"),
            outside_user) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => outside_user_not_associated_msg
                                      }
                                    })
        end
      end

      it 'returns a 404 ("Not Found") for missing node_state for admin' do
        get(api_url("/pushy/jobs/#{non_existent_job}"),
            admin_user) do |response|
          response.should look_like({
                                      :status => 404
                                    })
        end
      end

      it 'returns a 404 ("Not Found") for missing node_state for normal user' do

        get(api_url("/pushy/jobs/#{non_existent_job}"),
            normal_user) do |response|
          response.should look_like({
                                      :status => 404
                                    })
        end
      end
    end
  end
end
