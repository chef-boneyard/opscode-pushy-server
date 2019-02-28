name             'opscode-pushy-server'
maintainer       "Opscode, Inc."
maintainer_email "cookbooks@opscode.com"
license          "Apache 2.0"
description      "Installs/Configures opscode-pushy-server"
long_description IO.read(File.join(File.dirname(__FILE__), 'README.md'))
version          "0.2.0"

depends          'enterprise' # grabbed via Berkshelf + Git
