# Push Jobs Server Changelog

## Next
* Remove replaces line for RPM build 

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
