#
# Copyright 2014 Chef Software, Inc.
#
# All Rights Reserved.
#

name "opscode-pushy-server-ctl"

license :project_license

source path: "#{project.files_path}/pushy-server-ctl-commands"

dependency "omnibus-ctl"

build do
    block do
      erb source: "opscode-pushy-server-ctl.erb",
        dest:   "#{install_dir}/bin/opscode-push-jobs-server-ctl",
        mode:   0755,
        vars:   {
          embedded_bin: "#{install_dir}/embedded/bin",
          embedded_service: "#{install_dir}/embedded/service",
        }
    end

  # additional omnibus-ctl commands
  sync project_dir, "#{install_dir}/embedded/service/omnibus-ctl/"
end
