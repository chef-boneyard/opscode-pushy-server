# Chef Push Jobs Server Release Process

## Document Purpose

The purpose of this document is to describe the *current* release
process such that any member of the team can do a release.  As we
improve the automation around the release process, the document should
be updated such that it always has the exact steps required to release
Chef Push Jobs Server.

This document is NOT aspirational.  We have a number of automation
tools that we intend to use to improve the release release process;
however, many are not fully integrated with our process yet. Do not
add them to this document until they are ready to be part of the
release process and can be used by any team member to perform a
release.

## Pre-requisites

In order to release, you will need the following accounts/permissions:

- Push access to the opscode-pushy-server github repository
- Chef Software, Inc Slack account
- VPN account for Chef Software, Inc.
- Account on [https://discourse.chef.io](https://discourse.chef.io) using your Chef email address

## THE PROCESS

- All merges to master are automatically built by Buildkite and placed in the unstable channel

- Buildkite will run [very limited] tests and promote the built to the current channel

- To release a build, use `/expeditor promote` on Slack to promote the build to the stable channel

- Chef employees should already know a release is coming; however, as a
  courtesy, drop a message in the #cft-announce slack channel that the release
  is coming. Provide the release number and any highlights of the release.

- Write and then publish a Discourse post on https://discourse.chef.io
  once the release is live. This post should contain a link to the downloads
  page ([https://downloads.chef.io](https://downloads.chef.io)) and its contents
  should be based on the information that was added to the RELEASE\_NOTES.md file
  in an earlier step. *The post should  be published to the Chef Release
  Announcements category on https://discourse.chef.io. If it is a security
  release, it should also be published to the Chef Security Announcements
  category.* Full details on the policy of making release announcements on
  Discourse can be found on the wiki: [https://chefio.atlassian.net/wiki/display/ENG/Release+Announcements+and+Security+Alerts](https://chefio.atlassian.net/wiki/display/ENG/Release+Announcements+and+Security+Alerts)

- Let `#cft-announce` know about the release, including a link to the Discourse post.

Chef Push Jobs Server is now released.
