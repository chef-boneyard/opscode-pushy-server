# Push Jobs Server Release Notes

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
