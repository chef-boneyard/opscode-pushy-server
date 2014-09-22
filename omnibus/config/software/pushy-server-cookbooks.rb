#
# Copyright 2014 Chef Software, Inc.
#
# All Rights Reserved.
#

name "pushy-server-cookbooks"

dependency 'berkshelf2'

cookbook_name = "opscode-pushy-server"
cookbook_dir = "#{install_dir}/embedded/cookbooks"

source path: "#{project.files_path}/pushy-server-cookbooks"

build do
  mkdir "#{cookbook_dir}"
  command "cd #{cookbook_name} && #{install_dir}/embedded/bin/berks install" \
          " --path=#{cookbook_dir}"
  block do
    File.open("#{cookbook_dir}/dna.json", "w") do |f|
      f.write JSON.fast_generate(
        run_list: [
          'recipe[opscode-pushy-server::default]',
        ]
      )
    end
    File.open("#{cookbook_dir}/show-config.json", "w") do |f|
      f.write JSON.fast_generate(
        run_list: [
          'recipe[opscode-pushy-server::show_config]',
        ]
      )
    end
    File.open("#{cookbook_dir}/solo.rb", "w") do |f|
      f.write <<-EOH.gsub(/^ {8}/, '')
        cookbook_path   "#{install_dir}/embedded/cookbooks"
        file_cache_path "#{install_dir}/embedded/cookbooks/cache"
        verbose_logging true
      EOH
    end
  end
end
