# Push Jobs Server Release Notes

## 3.0

Versions of Chef Push Jobs 3.0 and later may only be used under the terms of the [Chef EULA](https://www.chef.io/end-user-license-agreement/) or another commercial agreement with Chef Software.

### Dependency updates
- Chef Infra Client 13.3.42 to 15.10.12

### Security updates

These updates include a large number of CVE fixes. Users are strongly encouraged to upgrade.

TBD

Added platform support:
- RHEL 8
- Ubuntu 20.04

Removed EOL platform support:
- RHEL 5
- Ubuntu 14.04

For more information see our [platform support policy](https://docs.chef.io/platforms/#platform-end-of-life-policy).

### experimental max\_body\_size setting

TBD

## 2.2.2

Security fix release for updating zlib to 1.2.11.

## 2.2.1

As mentioned in the 2.2.0 release notes, Push Jobs Server now requires
Chef Server 12.14.0. This release enforces that constraint in the
package metadata.

## 2.2.0

This release updates Push Jobs Server to ensure compatibility with
Chef Server 12.14.0 and later.  This includes enhancements to use
Chef Server's new unified credentials handling.

*Backwards Compatibility Note*

This version of Push Jobs Server is not compatible with any version of Chef Server earlier
than 12.14.0.

If you are using Chef Server 12.13.0 or earlier, do not upgrade this add-on
until after you have upgraded Chef Server to 12.14.0.

## 2.1.1
This release contains the latest patched version of OpenSSL (1.0.2j) as well as
various build, test, and development environment improvements.

It also fixes a bug where messages that exceed the allowed message size would
not be handled properly. Additional client fixes related to this bug are
in Push Jobs Client 2.1.4.

## 2.1.0
NOTE: at this time we do not recommend upgrading Automate to Push Server 2.1.0

* Substantial stability improvements in the tests.

* Upgrade to rebar 3

* Upgrade to Erlang 18

* Modernized the build and repo management to make building easier,
  merged in multiple separate repositories into one tree.

## 2.0.1

* Allow the job excution environment to be set; this includes user to
  be set, working directory, environment variables and a data file to
  be set.

* Command output capture. We now optionally capture stdout/stderr from job
  execution and return it to the server. Users can retrieve the output
  via the knife job output command.

* SSE feeds for job events. We now provide two SSE feed endpoints; one
  provides fine grained per-job events, while the other provides a
  per-org feed of jobs starting and completing.

* Use libsodium based encryption via zeromq4's CurveZMQ. This replaces
  the signing protocol used in 1.x. All zeromq packets are fully
  encrypted and signed, except for the server heartbeat broadcast,
  which is signed, but in the clear.

  This is not backwards compatibile with push 1.0 clients, and users
  should fully upgrade to 2.x push clients, and only then upgrade the server.

* Upgrade to Erlang 17.5


## 1.1.3 (2014-09-17)

### What's New:

* Remove hard dependency on `chef-server-core`

### Security Fixes:

* Updated Omnibus with default package user/group fix
* Ensure contents of install dir are owned by root

## 1.1.2 (2014-06-06)

### What's New:

* This is a security fix release, and has no new features.

### Security Fixes:

* [openssl] Patch for man in the middle attack, other vulnerabilities (CVE-2014-0224)

## 1.1.1 (2014-04-09)

### What's New:

The following items are new for Push Jobs Server 1.1.1 and/or are changes from previous versions:

* [core] Erlang r15b03-01 w/ multiple stability and bug fixes
* [core] Chef 11.10.4 (was 11.6.0)

### Security Fixes:

The following items are the set of security fixes that have been applied since Push Jobs Server 1.1.0:

* [openssl] Patch for heartbeat extension exposing process memory (CVE-2014-0160)
* [libyaml] Patch for arbitrary code execution vulnerability (CVE-2014-2525)
