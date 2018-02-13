# Push Jobs Server Changelog
<!-- usage documentation: http://expeditor-docs.es.chef.io/configuration/changelog/ -->
<!-- latest_release -->
<!-- latest_release -->

<!-- release_rollup -->
<!-- release_rollup -->

<!-- latest_stable_release -->
## [2.2.8](https://github.com/chef/opscode-pushy-server/tree/2.2.8) (2018-02-13)

#### Merged Pull Requests
- Use the version of LicenseScout that comes with the Omnibus gem. [#178](https://github.com/chef/opscode-pushy-server/pull/178) ([tduffield](https://github.com/tduffield))
- Update to ruby 2.4.3 [#177](https://github.com/chef/opscode-pushy-server/pull/177) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
<!-- latest_stable_release -->

## [2.2.6](https://github.com/chef/opscode-pushy-server/tree/2.2.6) (2017-09-21)

#### Merged Pull Requests
- [JEX-608] Use Expeditor to bump version and trigger release build [#170](https://github.com/chef/opscode-pushy-server/pull/170) ([schisamo](https://github.com/schisamo))
- Update the CHANGELOG when push-server is promoted to stable [#171](https://github.com/chef/opscode-pushy-server/pull/171) ([tduffield](https://github.com/tduffield))
- Allow opscode_pushy_server vip to be overridden [#172](https://github.com/chef/opscode-pushy-server/pull/172) ([OBrienCommaJosh](https://github.com/OBrienCommaJosh))
- Update to Ruby 2.4.2 and Chef 13 [#175](https://github.com/chef/opscode-pushy-server/pull/175) ([stevendanna](https://github.com/stevendanna))



## 2.2.2 (2016-06-26)
* Security fix release: update zlib to 1.2.11

## 2.2.1 (2016-04-04)
* Enforce chef-server version constraint in package metadata

## 2.2.0 (2016-03-30)
* use `chef_secrets` and `Veil` to manage credentials

## 2.1.1 (2016-12-01)
* Remove cURL dependencies
* Improvements to build, development, and test environments
* Upgrade runit
* Better handling of messages that exceed MAX_BODY_SIZE
* Update to latest OpenSSL

## 2.1.0 (2016-07-27)
* Substantial stability improvements in the tests
* Upgrade to Rebar3
* Upgrade to Erlang 18
* Modernize the build and repo management to make building easier,
  merge in multiple separate repositories into one tree

## 2.0.1 (2015-11-10)
* Release bump to make sure versions sort right.

## 2.0.0 (Never released except in alpha form)
* Added curve based encryption for all ZeroMQ communication
* Added parameter/environment variable/file transfer support
* Added output capture
* Added protocol version information
* Added Server Sent Event feeds for job execution.
* Work in progress to support external postgresql
* Upgrade omnibus software, including:
  * Updated to Chef 12.5.0 for configure
  * Erlang 17.5

## 1.3.0 (2015-06-21)
* Push server: Add support for multiple keys and updated principals endpoint
* Upgrade to ZeroMQ 4

## 1.2.0 (2015-06-09) (Unreleased)
* Push server: bring return codes for malformed headers into sync with chef server
* enterprise-chef-common: Update to 0.5.1 (from 0.3.0)
  Includes many HA improvments and refactoring for reusability, along
  with updates for systemd and runit
* Remove replaces line for RPM build
* Update openssl and other omnibus software defs.
* Update erlang to R16B03
* Allow client visible server name to be separate from the VIP
(https://github.com/chef/omnibus-pushy/issues/69)
* Fix permissions on opscode-push-jobs-server-running.json
* Add el7 support to test kitchen

## 1.1.6 (2014-12-04)
* Sign packages with Omnibus 4
* Update mixlib-shellout to 1.6.1

## 1.1.5 (2014-11-14)
* Update gem depenencies

### oc-pushy-pedant 1.0.8
* Do not test org creation by default. This fixes some backward
  compatibilty issues with Push Server and EC 11 which slipped in
  1.0.7

## 1.1.4 (2014-11-11)

### oc-pushy-pedant 1.0.7
* Update gem dependencies to chef-server-core current

## 1.1.3 (2014-09-17)

* Updated Omnibus with default package user/group fix
* Ensure contents of install dir are owned by root
* Remove hard dependency on `chef-server-core`
* Remove opscode-push-jobs-client

## 1.1.2 (2014-06-06)

### openssl 1.0.1f
* Upgrade to address CVE-2014-0224 and other vulnerabilites

### berkshelf
* Upgrade to version 2.0.15

## 1.1.1 (2014-04-09)

### berkshelf
* new dep: libffi
* new dep: libarchive

### chef
* upgrade to version 11.10.4

### erlang
* upgrade to r15b03-1

### nokigiri
* upgrade to nokigiri 1.6.1

### libyaml 0.1.6
* CVE-2014-2525: Heap-based buffer overflow allows context-dependent attackers to execute arbitrary code

### opscode-pushy-server 1.1.0
* Add Apache 2 license and headers in preparation for eventual open sourcing of Chef Push.
* Update sqerl config for latest sqerl.
* Update lock deps: eper, proper, lager, goldrush, rebar_lock_deps_plugin, pushy_common (license change)

### openssl 1.0.1g
* CVE-2014-0160: heartbeat extension allows remote attackers to obtain sensitive information from process memory

### pushy-server-cookbooks
* Increase postgresql max_connections to 350 to handle 4 node cluster
* Manage permissions for /var/log/opscode for non 0022 umasks

## 1.1.0 (2014-02-12)

* Add X-Chef-Version HTTP header information for compatibility with EC 11.1.0
* Change internal handling of principal endpoint responses for compatibility with EC 11.1.0
* Fix a bug that prevented Pushy from running properly on tiered EC topologies
* Update `libyaml` to address CVE-2013-6393
* Integration tests made more robust to timing issues

## 1.0.3 (2014-02-11)

Unpublished, following extended discussions on what our release
support policy should be.

Security release for a `libyaml` vulnerability.  Not compatible with
Enterprise Chef 11.1.0; users that are upgrading their Enterprise Chef
installations should use Pushy 1.1.0 instead.

* Update libyaml to address CVE-2013-6393

## 1.0.2 (2014-02-10)

Unpublished: superceded by 1.1.0.

1.0.2 was tagged prior to the decision to issue a new release
containing just the security patch for CVE-2013-6393.  It is
functionally equivalent to 1.1.0.  Due to incompatibilities with
Enterprise Chef 11.1.0, however, Pushy 1.0.2 was not published to
avoid confusion for users that were not yet prepared to upgrade their
Enterprise Chef servers.

## 1.0.1 (2013-11-01)

Initial Release

## 1.0.0 (2013-10-29)

Unpublished: mistagged release.