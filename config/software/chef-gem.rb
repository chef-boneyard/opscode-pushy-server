#
# Copyright 2014 Chef Software, Inc.
#
# All Rights Reserved.
#

name "chef-gem"
default_version "11.12.8"

dependency "ruby"
dependency "rubygems"
dependency "yajl"

build do
  gem "install chef" \
      " -v #{version}" \
      " -n #{install_dir}/bin" \
      " --no-rdoc" \
      " --no-ri"

  gem "install highline" \
      " -n #{install_dir}/bin" \
      " --no-rdoc" \
      " --no-ri"

  gem "install net-ssh-multi" \
      " -n #{install_dir}/bin" \
      " --no-rdoc" \
      " --no-ri"

  # clean up
  ["docs",
   "share/man",
   "share/doc",
   "share/gtk-doc",
   "ssl/man",
   "man",
   "info"].each do |dir|
    delete "#{install_dir}/embedded/#{dir}"
  end
end
