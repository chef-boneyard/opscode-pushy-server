#
# -*- indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
# ex: ts=4 sw=4 et
#
# Author:: Douglas Triggs (<doug@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#

require 'pedant/rspec/auth_headers_util'

describe "pushy config" do
  def self.ruby?
    false
  end

  let(:chef_server_host) {
    chef_server = Pedant.config[:chef_server]
    chef_server.gsub(/http[s]?:\/\//,'')
  }

  let(:config_body) {
    {
      "host" => chef_server_host,
      "public_key" => /^-----BEGIN PUBLIC KEY-----/,
      "type" => "config"
      # There are a bunch of additional values
      # We're not testing; one difficulty is that
      # Organization/node are returning arrays,
      # not actual strings, and push_jobs is complicated
    } }

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

  # TODO: should get this from config instead of hard-coding
  let(:config_name) { 'DONKEY' }

  let(:failed_to_authenticate_as_invalid_msg) {
    ["Failed to authenticate as 'invalid'. Ensure that your node_name and client key are correct."] }
  let(:outside_user_not_associated_msg) {
    ["'pedant-nobody' not associated with organization '#{org}'"] }

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
        get(api_url("/pushy/config/#{config_name}"),
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

      it 'returns a 403 ("Forbidden") for outside user', :pending do
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
    end
  end

  context 'invalid request' do
    it "returns 403 (\"Forbidden\") when organization doesn't exist", :pending do
      # This should be un-pended when OC-5484 is done
      path = api_url("/pushy/config/#{config_name}").gsub(org, "bogus-org")
      get(path, admin_user) do |response|
        response.should look_like({
                                    :status => 403
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
