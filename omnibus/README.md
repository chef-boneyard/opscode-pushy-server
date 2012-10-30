# Omnibus Pushy

This repository contains the skeleton for building Omnibus Push Jobs packages.

# Building opscode-push-jobs-server

    ./omnibus-build ubuntu-10.04

OR

    git clone git@github.com:opscode/omnibus-pushy.git
    git clone git@github.com:opscode/omnibus-software.git
    cd omnibus-pushy
    cp omnibus.rb.example omnibus.rb
    bundle install
    bundle exec vagrant omnibus build ubuntu-10.04 opscode-push-jobs-server

Packages will be in pkg/

## Licensing

See the LICENSE file for details.

Copyright: Copyright (c) 2012 Opscode, Inc.
License: Apache License, Version 2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.


