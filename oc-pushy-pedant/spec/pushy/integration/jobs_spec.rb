#
# -*- indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
# ex: ts=4 sw=4 et
#
# Author:: Douglas Triggs (<doug@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#

require 'pedant/rspec/common'
require 'pedant/rspec/auth_headers_util'
require 'pushy/support/jobs_util'
require 'pushy/support/authorization_groups_util'

describe "Jobs API Endpoint", :jobs do
  include_context "authorization_groups_util"

  def self.ruby?
    # Needed for pedant header checking

    false
  end

  # TODO: turns out this doesn't really matter; will we need to create it
  # at some point?
  let(:node_name) { 'DONKEY' }
  let(:nodes) { %w{DONKEY} }

  let(:job_to_run) {
    {
      'command' => 'sleep 1',
      'nodes' => nodes
    }
  }

  let(:non_existent_job) { 'not_a_number' }
  let(:non_admin_authorization_failed_msg) {
    ["User or client 'pedant_user' does not have access to that action on this server."] }
  let(:non_admin_client_authorization_failed_msg) {
    ["User or client 'pedant_non_admin_client' does not have access to that action on this server."] }
  let(:non_member_authorization_failed_msg) {
    ["User or client 'pedant_admin_user' does not have access to that action on this server."] }
  let(:non_member_client_authorization_failed_msg) {
    ["User or client 'pedant_admin_client' does not have access to that action on this server."] }
  let(:failed_to_authenticate_as_invalid_msg) {
    ["Failed to authenticate as 'invalid'. Ensure that your node_name and client key are correct."] }
  let(:outside_user_not_associated_msg) {
    ["'pedant-nobody' not associated with organization '#{org}'"] }

  describe 'access control with no pushy_job groups' do
    let(:job_path) {
      # This is evaluated at runtime, so there's always a (short-lived) job to
      # detect during the test

      post(api_url("/pushy/jobs"), admin_user, :payload => job_to_run) do |response|
        list = JSON.parse(response.body)
        list["uri"]
      end
    }

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
        get(api_url("/pushy/jobs/"), platform.non_admin_client) do |response|
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
    end # context 'GET /jobs'

    context 'POST /jobs' do
      it 'returns a 200 ("OK") for admin' do
        post(api_url("/pushy/jobs/"), admin_user, :payload => job_to_run) do |response|
          response.should look_like({
                                      :status => 201
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for normal user' do
        post(api_url("/pushy/jobs/"), normal_user, :payload => job_to_run) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => non_admin_authorization_failed_msg
                                      }
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for client' do
        post(api_url("/pushy/jobs/"), platform.non_admin_client,
             :payload => job_to_run) do |response|
          response.
            should look_like({
                               :status => 403,
                               :body_exact => {
                                 "error" => non_admin_client_authorization_failed_msg
                               }
                             })
        end
      end

      it 'returns a 401 ("Unauthorized") for invalid user' do
        post(api_url("/pushy/jobs"), invalid_user, :payload => job_to_run) do |response|
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
        post(api_url("/pushy/jobs"), outside_user, :payload => job_to_run) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => outside_user_not_associated_msg
                                      }
                                    })
        end
      end
    end # context 'POST /jobs'

    context 'GET /jobs/<name>' do
      it 'returns a 200 ("OK") for admin' do
        get(job_path, admin_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 200 ("OK") for normal user' do
        get(job_path, normal_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 200 ("OK") for client' do
        get(job_path, platform.non_admin_client) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 401 ("Unauthorized") for invalid user' do
        get(job_path,
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
        get(job_path,
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
    end # context 'GET /jobs/<name>'
  end # describe 'access control with no pushy_job groups'

  describe 'access control with pushy_job groups' do
    # Doing these in reverse for extra fun; this will guarantee it doesn't
    # "accidentally" work if the groups are missing
    let(:member) { normal_user }
    let(:non_member) { admin_user }
    let(:member_client) { platform.non_admin_client }
    let(:non_member_client) { platform.admin_client }

    let(:job_path) {
      # This is evaluated at runtime, so there's always a (short-lived) job to
      # detect during the test

      post(api_url("/pushy/jobs"), member, :payload => job_to_run) do |response|
        list = JSON.parse(response.body)
        list["uri"]
      end
    }

    before(:all) do
      setup_group("pushy_job_readers", [member.name], [member_client.name], [])
      setup_group("pushy_job_writers", [member.name], [member_client.name], [])
    end

    after(:all) do
      delete(api_url("/groups/pushy_job_readers"), superuser)
      delete(api_url("/groups/pushy_job_writers"), superuser)
    end
      
    context 'GET /jobs with pushy_job_readers' do
      it 'returns a 200 ("OK") for member' do
        get(api_url("/pushy/jobs/"), member) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member' do
        get(api_url("/pushy/jobs/"), non_member) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => non_member_authorization_failed_msg
                                      }
                                    })
        end
      end

      it 'returns a 200 ("OK") for member client' do
        get(api_url("/pushy/jobs/"), member_client) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member client' do
        get(api_url("/pushy/jobs/"), non_member_client) do |response|
          response.
            should look_like({
                               :status => 403,
                               :body_exact => {
                                 "error" => non_member_client_authorization_failed_msg
                               }
                             })
        end
      end
    end # context 'GET /jobs with pushy_job_readers'

    context 'POST /jobs with pushy_job_writers' do
      it 'returns a 200 ("OK") for member' do
        post(api_url("/pushy/jobs/"), member, :payload => job_to_run) do |response|
          response.should look_like({
                                      :status => 201
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member' do
        post(api_url("/pushy/jobs/"), non_member, :payload => job_to_run) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => non_member_authorization_failed_msg
                                      }
                                    })
        end
      end

      it 'returns a 200 ("OK") for member client' do
        post(api_url("/pushy/jobs/"), member_client,
             :payload => job_to_run) do |response|
          response.should look_like({
                                      :status => 201
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member client' do
        post(api_url("/pushy/jobs/"), non_member_client,
             :payload => job_to_run) do |response|
          response.
            should look_like({
                               :status => 403,
                               :body_exact => {
                                 "error" => non_member_client_authorization_failed_msg
                               }
                             })
        end
      end
    end # context 'POST /jobs with pushy_job_writers'

    context 'GET /jobs/<name> with pushy_job_readers' do
      it 'returns a 200 ("OK") for member' do
        get(job_path, member) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member' do
        get(job_path, non_member) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => non_member_authorization_failed_msg
                                      }
                                    })
        end
      end

      it 'returns a 200 ("OK") for member client' do
        get(job_path, member_client) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member client' do
        get(job_path, non_member_client) do |response|
          response.
            should look_like({
                               :status => 403,
                               :body_exact => {
                                 "error" => non_member_client_authorization_failed_msg
                               }
                             })
        end
      end
    end # context 'GET /jobs/<name> with pushy_job_readers'
  end # describe 'access control with pushy_job groups'

  describe 'access control with nested pushy_job groups' do
    # Doing these in reverse for extra fun; this will guarantee it doesn't
    # "accidentally" work if the groups are missing
    let(:member) { normal_user }
    let(:non_member) { admin_user }
    let(:member_client) { platform.non_admin_client }
    let(:non_member_client) { platform.admin_client }

    let(:job_path) {
      # This is evaluated at runtime, so there's always a (short-lived) job to
      # detect during the test

      post(api_url("/pushy/jobs"), member, :payload => job_to_run) do |response|
        list = JSON.parse(response.body)
        list["uri"]
      end
    }

    before(:all) do
      setup_group("nested_pushy_job_readers", [member.name], [member_client.name], [])
      setup_group("nested_pushy_job_writers", [member.name], [member_client.name], [])
      setup_group("pushy_job_readers", [], [], ["nested_pushy_job_readers"])
      setup_group("pushy_job_writers", [], [], ["nested_pushy_job_writers"])
    end

    after(:all) do
      delete(api_url("/groups/pushy_job_readers"), superuser)
      delete(api_url("/groups/pushy_job_writers"), superuser)
      delete(api_url("/groups/nested_pushy_job_readers"), superuser)
      delete(api_url("/groups/nested_pushy_job_writers"), superuser)
    end
      
    context 'GET /jobs with nested pushy_job_readers' do
      it 'returns a 200 ("OK") for member' do
        get(api_url("/pushy/jobs/"), member) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member' do
        get(api_url("/pushy/jobs/"), non_member) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => non_member_authorization_failed_msg
                                      }
                                    })
        end
      end

      it 'returns a 200 ("OK") for member client' do
        get(api_url("/pushy/jobs/"), member_client) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member client' do
        get(api_url("/pushy/jobs/"), non_member_client) do |response|
          response.
            should look_like({
                               :status => 403,
                               :body_exact => {
                                 "error" => non_member_client_authorization_failed_msg
                               }
                             })
        end
      end
    end # context 'GET /jobs with nested pushy_job_readers'

    context 'POST /jobs with nested pushy_job_writers' do
      it 'returns a 200 ("OK") for member' do
        post(api_url("/pushy/jobs/"), member, :payload => job_to_run) do |response|
          response.should look_like({
                                      :status => 201
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member' do
        post(api_url("/pushy/jobs/"), non_member, :payload => job_to_run) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => non_member_authorization_failed_msg
                                      }
                                    })
        end
      end

      it 'returns a 200 ("OK") for member client' do
        post(api_url("/pushy/jobs/"), member_client,
             :payload => job_to_run) do |response|
          response.should look_like({
                                      :status => 201
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member client' do
        post(api_url("/pushy/jobs/"), non_member_client,
             :payload => job_to_run) do |response|
          response.
            should look_like({
                               :status => 403,
                               :body_exact => {
                                 "error" => non_member_client_authorization_failed_msg
                               }
                             })
        end
      end
    end # context 'POST /jobs with nested pushy_job_writers'

    context 'GET /jobs/<name> with nested pushy_job_readers' do
      it 'returns a 200 ("OK") for member' do
        get(job_path, member) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member' do
        get(job_path, non_member) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => non_member_authorization_failed_msg
                                      }
                                    })
        end
      end

      it 'returns a 200 ("OK") for member client' do
        get(job_path, member_client) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for non-member client' do
        get(job_path, non_member_client) do |response|
          response.
            should look_like({
                               :status => 403,
                               :body_exact => {
                                 "error" => non_member_client_authorization_failed_msg
                               }
                             })
        end
      end
    end # context 'GET /jobs/<name> with nested pushy_job_readers'
  end # describe 'access control with nested pushy_job groups'

  describe 'request error checking' do
    let(:job_path) {
      # This is evaluated at runtime, so there's always a (short-lived) job to
      # detect during the test

      post(api_url("/pushy/jobs"), admin_user, :payload => job_to_run) do |response|
        list = JSON.parse(response.body)
        list["uri"]
      end
    }

    context 'invalid GET request' do
      it 'returns 403 ("Forbidden") with bogus org for /jobs' do
        path = api_url("/pushy/jobs").gsub(org, "bogus-org")
        get(path, admin_user) do |response|
          response.should look_like({
                                      :status => 403
                                    })
        end
      end

      it 'returns 403 ("Forbidden") with bogus org for /jobs/<name>' do
        path = job_path.gsub(org, "bogus-org")
        get(path, admin_user) do |response|
          response.should look_like({
                                      :status => 403
                                    })
        end
      end
    end

    context 'invalid POST request', :focus do
      include_context "job_body_util"

      it "returns 403 (\"Forbidden\") when organization doesn't exist" do
        path = api_url("/pushy/jobs").gsub(org, "bogus-org")
        post(path, admin_user, :payload => job_to_run) do |response|
          response.should look_like({
                                      :status => 403
                                    })
        end
      end

      context "for command" do
        succeeds_with_value("command", "")
        succeeds_with_value("command", "sleep 2")
        fails_with_value("command", :delete)
        fails_with_value("command", [])
        fails_with_value("command", {})
        fails_with_value("command", 0)
        fails_with_value("command", false)
      end

      context "for nodes" do
        succeeds_with_value("nodes", ["DONKEY"], {"unavailable" => ["DONKEY"]})
        succeeds_with_value("nodes", ["DONKEY", "FIONA"],
                            {"unavailable" => ["DONKEY", "FIONA"]})
        fails_with_value("nodes", :delete)
        fails_with_value("nodes", "")
        fails_with_value("nodes", "DONKEY")
        fails_with_value("nodes", [])
        fails_with_value("nodes", ["DONKEY", "FIONA", false])
        fails_with_value("nodes", ["DONKEY", "FIONA", nil])
        fails_with_value("nodes", ["DONKEY", "FIONA", {}])
        fails_with_value("nodes", {})
        fails_with_value("nodes", 0)
        fails_with_value("nodes", false)
      end

      context "quorum" do
        succeeds_with_value("quorum", :delete)
        succeeds_with_value("quorum", 1)
        succeeds_with_value("quorum", 999)
        fails_with_value("quorum", "")
        fails_with_value("quorum", "1")
        fails_with_value("quorum", [])
        fails_with_value("quorum", {})
        fails_with_value("quorum", false)
      end

      context "run_timeout" do
      end

      context "random shiznit" do
      end
    end

    describe 'handling authentication headers' do
      let(:method) { :GET }
      let(:body) { nil }
      let(:success_user) { admin_user }
      let(:failure_user) { invalid_user }

      context 'GET /jobs' do
        let(:url) { api_url("/pushy/jobs") }
        let(:response_should_be_successful) do
          response.should look_like({
                                      :status => 200
                                      # TODO: seems it's still not matching arrays
                                      # correctly; Didn't John fix this at some point?
                                    })
        end

        include_context 'handles authentication headers correctly'
      end

      context 'POST /jobs' do
        let(:method) { :POST }
        let(:body) { job_to_run }
        let(:url) { api_url("/pushy/jobs") }
        let(:response_should_be_successful) do
          response.should look_like({
                                      :status => 201,
                                      :body_exact => {
                                        'uri' => /^https\:\/\/.*\/jobs\/[0-9a-f]{32}$/
                                      }
                                    })
        end

        include_context 'handles authentication headers correctly'
      end

      context 'GET /jobs/<name>' do
        let(:url) { job_path }
        let(:response_should_be_successful) do
          response.should look_like({
                                      :status => 200,
                                      :body_exact => {
                                        'command' => 'sleep 1',
                                        'id' => /^[0-9a-f]{32}$/,
                                        'nodes' => {"unavailable" => ["DONKEY"]},
                                        'run_timeout' => 3600,
                                        'status' => 'quorum_failed'
                                      }
                                    })
        end

        include_context 'handles authentication headers correctly'
      end
    end # describe 'handling authentication headers'
  end # describe 'input error checking'
end # describe "Jobs API Endpoint"
