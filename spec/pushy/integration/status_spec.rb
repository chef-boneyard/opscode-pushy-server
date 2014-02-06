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

describe "pushy status" do
  let(:path) { api_url("/_status").gsub(/organizations\/.*\//, '/pushy/') }

  it 'PUT /pushy/_status returns 405 ("Method Not Allowed")' do
    put(path, admin_user, :payload => {}) do |response|
      response.should look_like({
          :status => 405
        })
    end
  end

  it 'POST /pushy/_status returns 405 ("Method Not Allowed")' do
    post(path, admin_user, :payload => {}) do |response|
      response.should look_like({
          :status => 405
        })
    end
  end

  it 'DELETE /pushy/_status returns 405 ("Method Not Allowed")' do
    delete(path, admin_user) do |response|
      response.should look_like({
          :status => 405
        })
    end
  end

  describe 'access control' do
    context 'GET /pushy/_status' do
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
