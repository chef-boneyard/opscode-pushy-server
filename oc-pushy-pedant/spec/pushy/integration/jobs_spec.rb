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
require 'pushy/support/validation_util'
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
    ["'pedant-nobody' is not associated with organization '#{org}'"] }

  describe 'HTTP verb validation' do
    context '/organizations/<org>/pushy/jobs/' do
      it 'PUT returns a 405 ("Method Not Allowed")' do
        put(api_url("/pushy/jobs/"), admin_user, :payload => job_to_run) do |response|
          response.should look_like({
              :status => 405
            })
        end
      end

      it 'DELETE returns a 405 ("Method Not Allowed")' do
        delete(api_url("/pushy/jobs/"), admin_user) do |response|
          response.should look_like({
              :status => 405
            })
        end
      end
    end # context '/organizations/<org>/pushy/jobs/'

    context '/organizations/<org>/pushy/jobs/<name>' do
      context 'with job <name>' do
        let(:job_path) {
          # This is evaluated at runtime, so there's always a (short-lived) job to
          # detect during the test

          post(api_url("/pushy/jobs"), admin_user, :payload => job_to_run) do |response|
            list = JSON.parse(response.body)
            list["uri"]
          end
        }

        it 'PUT returns a 405 ("Method Not Allowed")' do
          put(job_path, admin_user, :payload => job_to_run) do |response|
            response.should look_like({
                :status => 405
              })
          end
        end

        it 'POST returns a 405 ("Method Not Allowed")' do
          post(job_path, admin_user, :payload => job_to_run) do |response|
            response.should look_like({
                :status => 405
              })
          end
        end

        it 'DELETE returns a 405 ("Method Not Allowed")' do
          delete(job_path, admin_user) do |response|
            response.should look_like({
                :status => 405
              })
          end
        end
      end # context with job <name>

      context 'where <name> does not exist' do
        let(:job_path) {
          api_url("/pushy/jobs/name-does-not-exist")
        }

        it 'GET returns a 404 ("Not Found")' do
          get(job_path, admin_user) do |response|
            response.should look_like({
                :status => 404
              })
          end
        end

        it 'PUT returns a 405 ("Method Not Allowed")' do
          put(job_path, admin_user, :payload => job_to_run) do |response|
            response.should look_like({
                :status => 405
              })
          end
        end

        it 'POST returns a 405 ("Method Not Allowed")' do
          post(job_path, admin_user, :payload => job_to_run) do |response|
            response.should look_like({
                :status => 405
              })
          end
        end

        it 'DELETE returns a 405 ("Method Not Allowed")' do
          delete(job_path, admin_user) do |response|
            response.should look_like({
                :status => 405
              })
          end
        end
      end # where <name> does not exist
    end # context '/organizations/<org>/pushy/jobs/<name>'
  end # describe 'HTTP verb validation'

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

      it 'returns a 403 ("Forbidden") for outside user' do
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

      it 'returns a 401 ("Unauthorized") for bogus client' do
        get(api_url("/pushy/jobs/"), platform.bad_client) do |response|
          response.should look_like({
                                      :status => 401
                                    })
        end
      end
    end # context 'GET /jobs'

    context 'POST /jobs' do
      it 'returns a 201 ("OK") for admin' do
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

      it 'returns a 403 ("Forbidden") for outside user' do
        post(api_url("/pushy/jobs"), outside_user, :payload => job_to_run) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => outside_user_not_associated_msg
                                      }
                                    })
        end
      end

      it 'returns a 401 ("Unauthorized") for bogus client' do
        post(api_url("/pushy/jobs"), platform.bad_client,
             :payload => job_to_run) do |response|
          response.should look_like({
                                      :status => 401
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

      it 'returns a 403 ("Forbidden") for outside user' do
        get(job_path, outside_user) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => outside_user_not_associated_msg
                                      }
                                    })
        end
      end

      it 'returns a 401 ("Unauthorized") for bogus client' do
        get(job_path, platform.bad_client) do |response|
          response.should look_like({
                                      :status => 401
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
      setup_group("pushy_job_readers", [member.name, outside_user.name],
                  [member_client.name], [])
      setup_group("pushy_job_writers", [member.name, outside_user.name],
                  [member_client.name], [])
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

      it 'returns a 403 ("Forbidden") for outside user' do
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
    end # context 'GET /jobs with pushy_job_readers'

    context 'POST /jobs with pushy_job_writers' do
      it 'returns a 201 ("OK") for member' do
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

      it 'returns a 201 ("OK") for member client' do
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

      it 'returns a 403 ("Forbidden") for outside user' do
        post(api_url("/pushy/jobs"), outside_user,
            :payload => job_to_run) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => outside_user_not_associated_msg
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

      it 'returns a 403 ("Forbidden") for outside user' do
        get(job_path, outside_user) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => outside_user_not_associated_msg
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
      it 'returns a 201 ("OK") for member' do
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

      it 'returns a 201 ("OK") for member client' do
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
      it 'returns 404 ("Not Found") with bogus org for /jobs' do
        path = api_url("/pushy/jobs").gsub(org, "bogus-org")
        get(path, admin_user) do |response|
          response.should look_like({
                                      :status => 404
                                    })
        end
      end

      it 'returns 404 ("Not Found") with bogus org for /jobs/<name>' do
        path = job_path.gsub(org, "bogus-org")
        get(path, admin_user) do |response|
          response.should look_like({
                                      :status => 404
                                    })
        end
      end
    end

    context 'invalid POST request' do
      include_context "job_body_util"

      it "returns 404 (\"Not Found\") when organization doesn't exist" do
        path = api_url("/pushy/jobs").gsub(org, "bogus-org")
        post(path, admin_user, :payload => job_to_run) do |response|
          response.should look_like({
                                      :status => 404
                                    })
        end
      end

      context "for command" do
        succeeds_with_value("command", "", nil, true) # pend this for transient mystery failures on CentOS :(
        succeeds_with_value("command", "sleep 2")
        fails_with_value("command", :delete)
        fails_with_value("command", [])
        fails_with_value("command", {})
        fails_with_value("command", 0)
        fails_with_value("command", false)
        fails_with_value("command", nil)
      end

      context "for nodes" do
        succeeds_with_value("nodes", ["DONKEY"], {"unavailable" => ["DONKEY"]}, true) # pend this for transient mystery failures on CentOS :(
        succeeds_with_value("nodes", ["DONKEY", "FIONA"],
                            {"unavailable" => ["DONKEY", "FIONA"]}, true) # pend this for transient mystery failures on CentOS :(
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
        fails_with_value("nodes", nil)
      end

      context "quorum" do
        succeeds_with_value("quorum", :delete)
        succeeds_with_value("quorum", 1)
        # TODO: Might want to make this a failure with one node?:
        succeeds_with_value("quorum", 999)
        fails_with_value("quorum", "")
        fails_with_value("quorum", "1")
        fails_with_value("quorum", [])
        fails_with_value("quorum", {})
        fails_with_value("quorum", false)
        fails_with_value("quorum", nil)
      end

      context "run_timeout" do
        succeeds_with_value("run_timeout", :delete)
        # TODO: Might want to make this a failure for 0?:
        succeeds_with_value("run_timeout", 0)
        succeeds_with_value("run_timeout", 3600)
        fails_with_value("run_timeout", "")
        fails_with_value("run_timeout", "1")
        fails_with_value("run_timeout", [])
        fails_with_value("run_timeout", {})
        fails_with_value("run_timeout", false)
        fails_with_value("run_timeout", nil)
      end

      context "random shiznit" do
        fails_with_value("foo", nil, true)
        fails_with_value("foo", "", true)
        fails_with_value("foo", "bar", true)
        fails_with_value("foo", 0, true)
      end
    end

    describe 'handling authentication headers' do
      let(:method) { :GET }
      let(:body) { nil }
      let(:success_user) { admin_user }
      let(:failure_user) { invalid_user }

      context 'GET /jobs', :pending do # pend for transient failures until we can fix it for real :(
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
        include_context "validation_util"
        let(:url) { job_path }
        let(:response_should_be_successful) do
          response.should look_like({
                                      :status => 200,
                                      :body_exact => {
                                        'command' => 'sleep 1',
                                        'id' => /^[0-9a-f]{32}$/,
                                        'nodes' => {"unavailable" => ["DONKEY"]},
                                        'run_timeout' => 3600,
                                        'status' => 'quorum_failed',
                                        'created_at' => valid_datetime,
                                        'updated_at' => valid_datetime
                                      }
                                    })
        end

        include_context 'handles authentication headers correctly'
      end
    end # describe 'handling authentication headers'
  end # describe 'input error checking'

  describe 'provides timestamps in API responses' do
    include_context "validation_util"
    let(:job_path) {
      # This is evaluated at runtime, so there's always a (short-lived) job to
      # detect during the test

      post(api_url("/pushy/jobs"), admin_user, :payload => job_to_run) do |response|
        job = parse(response)
        job["uri"]
      end
    }

    context 'GET /jobs/<name>' do
      it 'returns created_at' do
        get(job_path, admin_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
          json = parse(response)
          validate_datetime json['created_at']
        end
      end

      it 'returns updated_at' do
        get(job_path, admin_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
          json = parse(response)
          validate_datetime json['updated_at']
        end
      end

      it 'has an updated_at later than or equal to created_at' do
        get(job_path, admin_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
          json = parse(response)
          validate_time_ordering(json['created_at'], json['updated_at'])
        end

      end
    end
  end
end # describe "Jobs API Endpoint"
