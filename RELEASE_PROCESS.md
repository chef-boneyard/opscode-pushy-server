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

- Local checkouts of the opsocde-pushy-server and chef-web-downloads repositories
- Push access to the opscode-pushy-server github repository
- Chef Software, Inc Slack account
- VPN account for Chef Software, Inc.
- Account on [https://discourse.chef.io](https://discourse.chef.io) using your Chef email address
- Login for wilson.ci.chef.co (This is linked to your github account.)
- Access to artifactory.chef.co
- Access to delivery.chef.co

## THE PROCESS
### Informing everyone of a pending release.

- [ ] Announce your intention to drive the release to #cft-announce on slack.
- [ ] Cross-post this intention on #chef-server and #spool.  Determine whether
  this upgrade requires a major/minor/patch bump and what the major changes
  are.
- [ ] Ensure that the opscode-push-jobs-server pipeline on wilson is currently green - you
  cannot ship if master is broken.

### Preparing for the release

- [ ] Update CHANGELOG.md to contain all of the changes needed in this release.
- [ ] Check RELEASE_NOTES.md to ensure that it describes the
  most important user-facing changes in the release. This file should
  form the basis of the post to Discourse that comes in a later step. Update as
  appropriate.  If a there is a major version change, transfer all
  contents to PRIOR_RELEASE_NOTES.md before adding release notes.
- [ ] Open a PR with these changes and post the link to #chef-server-rfr
  and #spool.

### Building and Releasing the Release

- [ ] Tag the opscode-pushy-server repository with the release version: `git
  tag -a VERSION_NUMBER`. The first line of the tag message should be
  the version number. The other lines are up to you. You're the boss!
  (You can leave them blank).

- [ ] Push the new tag: `git push origin master --tags`.

- [ ] Trigger a release build in Jenkins using the
  `opscode-push-jobs-server-trigger-release` trigger.  Use the tag you created
  above as the GIT_REF parameter.

- [ ] Wait for the pipeline to complete.

- [ ] Use julia to promote the build:
- `@julia artifactory promote opscode-push-jobs-server VERSION`. Please do this
- in the `#eng-services-support` room.  Once this is done, the release is
  available to the public via the APT and YUM repositories.

- [ ] Chef employees should already know a release is coming; however, as a
  courtesy, drop a message in the #cft-announce slack channel that the release
  is coming. Provide the release number and any highlights of the release.

- [ ] In your local checkout of the chef-web-downloads repository,
generate an update to the download pages using rake:

```
git co -b YOUR_INITIALS/release-push-server-VERSION
rake fetch[opscode-push-jobs-server]
git add data/
# make sure all the changes are what you expect
# write a simple commit message
git commit -v
```

- [ ] Create a delivery change request. See the README in
  chef-web-downloads for setup instructions if this doesn't work.

```
delivery review
```

- [ ] Have someone review and approve the change request.

- [ ] Once committed to master, an automated process will make the
  change to the live website; however, it might take a few minutes for
  the deploy to complete and then a few more for CDN caches to expire.

- [ ] Write and then publish a Discourse post on https://discourse.chef.io
  once the release is live. This post should contain a link to the downloads
  page ([https://downloads.chef.io](https://downloads.chef.io)) and its contents
  should be based on the information that was added to the RELEASE_NOTES.md file
  in an earlier step. *The post should  be published to the Chef Release
  Announcements category on https://discourse.chef.io. If it is a security
  release, it should also be published to the Chef Security Announcements
  category.* Full details on the policy of making release announcements on
  Discourse can be found on the wiki under the Engineering section ->
  Policy and Processes -> Release Announcements and Security Alerts

- [ ] Let `#cft-announce` know about the release, including a link to the Discourse post.

Chef Push Jobs Server is now released.
