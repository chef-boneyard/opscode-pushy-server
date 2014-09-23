#
# Copyright 2014 Chef Software, Inc.
#
# All Rights Reserved.
#

name "pushy-server-scripts"

source path: "#{project.files_path}/pushy-server-scripts"

build do
  sync project_dir, "#{install_dir}/bin/"
end
