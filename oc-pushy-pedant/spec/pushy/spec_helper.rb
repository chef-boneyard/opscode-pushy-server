# @copyright Copyright 2014 Chef Software, Inc. All Rights Reserved.
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

$:.unshift File.expand_path("../../lib", __FILE__)
$:.unshift File.expand_path("../..", __FILE__)

require 'bundler'
Bundler.require(:default, :test)

#require 'pushy-client'

require 'tmpdir'
require 'tempfile'

require 'pushy/support/end_to_end_util'
require 'pushy/support/validation_util'
require 'pushy/support/sse_util'

WATCH = lambda { |x| puts x } unless defined?(WATCH)

# Load everything from spec/support
# Do not change the gsub.
Dir["pushy/support/**/*.rb"].map { |f| f.gsub(%r{.rb$}, '') }.each { |f| require f }

#
# Hack to work around the inability to pass chef client config into
# push client startup. Long term we need to modify push client config
# and Chef::HTTP::DefaultSSLPolicy and APISSLPolicy to accept options
# from other sources.
#
puts "="*10 + "SSL verify mode hardcoded to verify_none" + "="*10
Chef::Config[:ssl_verify_mode] = :verify_none
Chef::Config[:verify_api_cert] = false
