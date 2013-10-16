#
# Copyright:: Copyright (c) 2012 Opscode, Inc.
# License:: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

name "pushy-server-cookbooks"

dependency 'berkshelf'

cookbook_name = "opscode-pushy-server"
cookbook_dir = "#{install_dir}/embedded/cookbooks"

source :path => File.expand_path("files/pushy-server-cookbooks", Omnibus.project_root)

build do
  command "mkdir -p #{cookbook_dir}"
  command "cd #{cookbook_name} && #{install_dir}/bin/berks install -c ./Berksfile --path=#{cookbook_dir}",
          :env => { "RUBYOPT"         => nil,
                    "BUNDLE_BIN_PATH" => nil,
                    "BUNDLE_GEMFILE"  => nil,
                    "GEM_PATH"        => nil,
                    "GEM_HOME"        => nil } # Manual "bundler buster" needed, because #command doesn't do it for us
  block do
    File.open("#{cookbook_dir}/dna.json", "w") do |f|
      f.puts "{\"run_list\": [ \"recipe[#{cookbook_name}]\" ]}"
    end
    File.open("#{cookbook_dir}/show-config.json", "w") do |f|
      f.puts "{\"run_list\": [ \"recipe[#{cookbook_name}::show_config]\" ]}"
    end
    File.open("#{cookbook_dir}/solo.rb", "w") do |f|
      f.puts "CURRENT_PATH = File.expand_path(File.dirname(__FILE__))"
      f.puts "file_cache_path \"\#\{CURRENT_PATH\}/cache\""
      f.puts "cookbook_path CURRENT_PATH"
      f.puts "verbose_logging true"
    end
  end
end
