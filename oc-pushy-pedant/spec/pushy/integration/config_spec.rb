#
# -*- indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
# ex: ts=4 sw=4 et
#
# Author:: Douglas Triggs (<doug@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#

describe "pushy config" do
  context "GET /organization/<name>/pushy/config/<nodename>" do
    it "returns 200 and server config" do
      get(api_url("pushy/config/DONKEY"), admin_user) do |response|
        chef_server = Pedant.config[:chef_server]
        chef_server_host = chef_server.gsub(/http[s]?:\/\//,'')
        # TODO: probably should grab the correct values from config instead of
        # hard-coding this, but this is proof-of-concept test only right now
        response.should look_like({
                                    :status => 200,
                                    :body => {
                                      "host" => chef_server_host,
                                      "public_key" => /^-----BEGIN PUBLIC KEY-----/,
                                      "type" => "config"
                                      # There are a bunch of additional values
                                      # We're not testing; one difficulty is that
                                      # Organization/node are returning arrays,
                                      # not actual strings, and push_jobs is complicated
                                    }
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

  describe 'access control', :focus do
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

      it 'returns a 200 ("OK") for admin client' do
        get(api_url("/pushy/config/#{config_name}"), platform.admin_client) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 200 ("OK") for non-admin client', :pending do
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
    end
  end
end # describe "pushy config"
