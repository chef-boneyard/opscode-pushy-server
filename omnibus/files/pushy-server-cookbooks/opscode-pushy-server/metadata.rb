name             'opscode-pushy-server'
maintainer       'Chef Software Inc Engineering'
maintainer_email 'engineering@chef.io'
license          "Apache 2.0"
description      "Installs/Configures opscode-pushy-server"
long_description IO.read(File.join(File.dirname(__FILE__), 'README.md'))
version          "0.2.0"

depends          'enterprise' # grabbed via Berkshelf + Git
