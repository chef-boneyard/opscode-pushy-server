#
# Copyright 2014 Chef Software, Inc.
#
# All Rights Reserved.
#

name "pushy-server-cookbooks"

source path: "#{project.files_path}/pushy-server-cookbooks"

dependency 'berkshelf2'

build do
  env = with_standard_compiler_flags(with_embedded_path)
  command "berks install" \
        " --path=#{install_dir}/embedded/cookbooks", env: env, cwd: "#{project_dir}/opscode-pushy-server"
  block do
    File.open("#{install_dir}/embedded/cookbooks/dna.json", "w") do |f|
      f.write JSON.fast_generate(
        run_list: [
          'recipe[opscode-pushy-server::default]',
        ]
      )
    end
    File.open("#{install_dir}/embedded/cookbooks/show-config.json", "w") do |f|
      f.write JSON.fast_generate(
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
