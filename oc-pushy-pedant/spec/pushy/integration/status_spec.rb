#
# -*- indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
# ex: ts=4 sw=4 et
#
# Author:: Douglas Triggs (<doug@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#

describe "pushy status" do
  describe 'access control' do
    context 'GET /_status' do

      let(:path) { api_url("/_status").gsub(/organizations\/.*\//, '') }

      it 'returns a 200 ("OK") for admin' do
        get(path, admin_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 200 ("OK") for normal user' do
        get(path, normal_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 200 ("OK") for client' do
        get(path, platform.admin_client) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 200 ("OK") for invalid user' do
        get(path, invalid_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 200 ("OK") for outside user' do
        get(path, outside_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end
    end
  end
end # describe "pushy status"
