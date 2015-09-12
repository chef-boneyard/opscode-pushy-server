source "https://rubygems.org"

gem "chef"

gemspec

# avoid some issues with ssl verification; find a better way ASAP
gem 'rest-client', :git => "https://github.com/chef/rest-client.git", :tag => ">=1.6.7"

# Note that oc-chef-pedant is a subdir of chef-server
gem 'oc-chef-pedant', :git => "https://github.com/chef/chef-server.git", :tag => '12.1.0-rc.3'
#
# floating pushy-client on master as an experiment
gem 'opscode-pushy-client', :git => "https://github.com/chef/opscode-pushy-client.git"
