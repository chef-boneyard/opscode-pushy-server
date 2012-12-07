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

      # TODO: currently all tests are pending, because the _status endpoint can't
      # be reached at all with the current combination of pushy and nginx routing

      let(:path) { api_url("/_status").gsub(/organizations\/.*\//, '') }

      it 'returns a 200 ("OK") for admin', :pending do
        get(path, admin_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 200 ("OK") for normal user', :pending do
        get(path, normal_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 200 ("OK") for client', :pending do
        get(path, client) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 200 ("OK") for invalid user', :pending do
        get(path, invalid_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end

      it 'returns a 200 ("OK") for outside user', :pending do
        get(path, outside_user) do |response|
          response.should look_like({
                                      :status => 200
                                    })
        end
      end
    end
  end
end # describe "pushy status"
