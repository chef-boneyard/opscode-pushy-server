source "https://rubygems.org"

gem "chef"

gemspec

# avoid some issues with ssl verification; find a better way ASAP
gem 'rest-client', :git => "https://github.com/chef/rest-client.git", :tag => ">=1.6.7"

# Note that oc-chef-pedant is a subdir of chef-server
#gem 'oc-chef-pedant', :git => "https://github.com/chef/chef-server.git", :tag => '12.1.0-rc.3'

# This is a sha on master, can advance to next tag when that happens.
gem 'oc-chef-pedant', :git => "https://github.com/chef/chef-server.git", :tag => "e465e0a3d11d63b0a216e73823767c03efca1b55"
#
# floating pushy-client on master as an experiment
gem 'opscode-pushy-client', :git => "https://github.com/chef/opscode-pushy-client.git", :tag=>'2.0.0'

