# opscode-push-jobs Omnibus project

This project creates full-stack platform-specific packages for `opscode-push-jobs`!

## Installation

We'll assume you have Ruby 1.9+ and Bundler installed. First ensure all
required gems are installed and ready to use:

```shell
$ bundle install --binstubs
```

## Usage

Kitchen-based Build Environment
-------------------------------
Every Omnibus project ships will a project-specific
[Berksfile](http://berkshelf.com/) that will allow you to build your omnibus projects on all of the projects listed in the `.kitchen.yml`. You can add/remove additional platforms as needed by changing the list found in the `.kitchen.yml` `platforms` YAML stanza.

This build environment is designed to get you up-and-running quickly. However,
there is nothing that restricts you to building on other platforms. Simply use
the [omnibus cookbook](https://github.com/opscode-cookbooks/omnibus) to setup
your desired platform and execute the build steps listed above.

The default build environment requires Test Kitchen and VirtualBox for local
development. Test Kitchen also exposes the ability to provision instances using
various cloud providers like AWS, DigitalOcean, or OpenStack. For more
information, please see the [Test Kitchen documentation](http://kitchen.ci).

Once you have tweaked your `.kitchen.yml` (or `.kitchen.local.yml`) to your
liking, you can bring up an individual build environment using the `kitchen`
command.

**NOTE:** Test Kitchen should be installed external to the local Ruby bundle.
Please either use ChefDK or install the latest test-kitchen from Rubygems.

```shell
kitchen converge default-ubuntu-1204
```

Then login to the instance and build the project as described in the Usage
section:

```shell
kitchen login default-ubuntu-1204

source load-omnibus-toolchain.sh
cd chef-push-server/omnibus
sudo bundle install
bundle exec omnibus build opscode-push-jobs-server
```

For a complete list of all commands and platforms, run `kitchen list` or
`kitchen help`.


### Build

You create a platform-specific package using the `build project` command:

```shell
$ bin/omnibus build opscode-push-jobs-server
```

The platform/architecture type of the package created will match the platform
where the `build` command is invoked. So running this command on a
MacBook Pro will generate a Mac OS X specific package. After the build
completes packages will be available in `pkg/`.

### Clean

You can clean up all temporary files generated during the build process with
the `clean` command:

```shell
$ bin/omnibus clean
```

Adding the `--purge` purge option removes __ALL__ files generated during the
build including the project install directory (`/opt/opscode`) and
the package cache directory (`/var/cache/omnibus/pkg`):

```shell
$ bin/omnibus clean --purge
```

### Cache

Lists source packages that are required but not yet cached:

```shell
$ bin/omnibus cache missing
```

Populate the S3 Cache:

```shell
$ bin/omnibus cache populate
```

### Publish

Omnibus has a built-in mechanism for releasing to a variety of "backends", such
as Amazon S3 and Artifactory. You must set the proper credentials in your `omnibus.rb`
config file or specify them via the command line.

```shell
$ bundle exec omnibus publish path/to/*.deb --backend s3
```

### Help

Full help for the Omnibus command line interface can be accessed with the
`help` command:

```shell
$ bin/omnibus help
```
## License

All files in the repository are licensed under the Apache 2.0 license. If any
file is missing the License header it should assume the following is attached;

Copyright 2014 Chef Software Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

