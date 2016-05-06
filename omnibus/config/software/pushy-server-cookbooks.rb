#
# Copyright 2012-2014 Chef Software, Inc.
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

license "Apache-2.0"
license_file "LICENSE"

source path: "#{project.files_path}/pushy-server-cookbooks"

dependency 'berkshelf2'

build do
  env = with_standard_compiler_flags(with_embedded_path)
  command "berks install" \
        " --path=#{install_dir}/embedded/cookbooks", env: env, cwd: "#{project_dir}/opscode-pushy-server"
  block do
    File.open("#{install_dir}/embedded/cookbooks/dna.json", "w") do |f|
      f.write FFI_Yajl::Encoder.encode(
        run_list: [
          'recipe[opscode-pushy-server::default]',
        ]
      )
    end
    File.open("#{install_dir}/embedded/cookbooks/show-config.json", "w") do |f|
      f.write FFI_Yajl::Encoder.encode(
        run_list: [
          'recipe[opscode-pushy-server::show_config]',
        ]
      )
    end
    File.open("#{install_dir}/embedded/cookbooks/solo.rb", "w") do |f|
      f.write <<-EOH.gsub(/^ {8}/, '')
        cookbook_path   "#{install_dir}/embedded/cookbooks"
        file_cache_path "#{install_dir}/embedded/cookbooks/cache"
        verbose_logging true
      EOH
    end
  end
end
