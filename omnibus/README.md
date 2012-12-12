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

## Overrides

For testing and CI purposes, it is sometimes convenient to selectively
override the installed version of a particular software package
without having to commit changes to software descriptors (i.e.,
`config/software/$SOFTWARE.rb` files).  To do this, place a file named
`omnibus.overrides` in the root of this repository prior to a build.
The format is a simple, plain-text one; each line contains a software
name and version, separated by whitespace.  There are no comments, no
leading whitespace, and no blank lines.  For example:

```
opscode-pushy-server my/branch
oc-pushy-pedant deadbeef
```

The software name must match the name given in the corresponding
software descriptor file, and the version can be anything accepted by
Omnibus as a valid version (e.g., branch name, tag name, SHA1, etc.)

If present, the versions of the software packages in this file will
supercede versions in the corresponding software descriptor file.
Additionally, the information in the generated
`/opt/opscode-push-jobs-server/version-manifest.txt` file (installed by the
generated installer) will indicate which (if any) packages had their
versions overridden, and what the version would have been if it hadn't
been overridden.

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


