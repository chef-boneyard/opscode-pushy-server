# RUNNING MIGRATIONS #
Cheat Sheet:

0. Install gems

      bundle install --binstubs

1. Create a db.yaml file like the following:

      adapter: mysql2
      host: localhost
      database: opscode_chef
      user: root
      password: TOPSECRETHERE

2. Now you can run the migration like so:

      bin/sequel -m db/migrate db.yaml

Read moar: <http://sequel.rubyforge.org/rdoc/files/doc/migration_rdoc.html>

# Modifying Schema #

Read here first:
https://wiki.corp.opscode.com/display/CORP/Chef+SQL+Schema

# Private Chef opscode-dev-vm  Notes #

Mount your local host-based copy of `mixlib-authorization` in the Private Chef
guest:

    helsinki:~OC/opscode-dev-vm$ rake project:load[mixlib-authorization]
    [vagrant] name => mixlib-authorization
    [vagrant] host path => /Users/schisamo/dev/code/opscode/mixlib-authorization
    [vagrant] guest path => /opt/opscode/embedded/service/mixlib-authorization
    [default] Mounting host directory '/Users/schisamo/dev/code/opscode/mixlib-authorization' as guest directory '/srv/piab/mounts/mixlib-authorization'
    [vagrant] Creating shared folders metadata...
    [vagrant] Mounting shared folders...
    [vagrant] -- mixlib-authorization: /srv/piab/mounts/mixlib-authorization
    [default] stdin: is not a tty
    [default] Fetching source index for http://rubygems.org/
    [default] Using rake (0.9.2)
    [default] Using activesupport (3.0.9)
    [default]
    [default] Using builder (2.1.2)
    [default] Using i18n (0.5.0)
    ...
    [default] Using rspec (2.6.0)
    [default] Using uuid (2.3.4)
    [default] Using bundler (1.0.22)
    [default] Your bundle is complete! It was installed into /opt/opscode/embedded/service/gem

SSH into the Private Chef guest:

    helsinki:~OC/opscode-dev-vm$ rake ssh
    Linux private-chef.opscode.piab 2.6.32-38-server #83-Ubuntu SMP Wed Jan 4 11:26:59 UTC 2012 x86_64 GNU/Linux
    Ubuntu 10.04.4 LTS

    Welcome to the Ubuntu Server!
     * Documentation:  http://www.ubuntu.com/server/doc
    Last login: Mon Mar 12 14:36:32 2012 from 10.0.2.2
    vagrant@private-chef:~$

Ensure the guest's `PATH` is set correctly (this should be set by a Bonfire
`after_start` callback):

    vagrant@private-chef:~$ echo $PATH
    /opt/opscode/embedded/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/opt/ruby/bin

If `/opt/opscode/embedded/bin` is not listed first in the `PATH` make it so:

    export PATH=/opt/opscode/embedded/bin:$PATH

Change to `mixlib-authorization` directory and run migrations:

    vagrant@private-chef:~$ cd /opt/opscode/embedded/service/mixlib-authorization
    vagrant@private-chef:/opt/opscode/embedded/service/mixlib-authorization$ bundle exec sequel -m db/migrate postgres://opscode-pgsql@127.0.0.1/opscode_chef_test

Verify everything migrated correctly:

    vagrant@private-chef:/opt/opscode/embedded/service/mixlib-authorization$ psql opscode_chef_test opscode-pgsql
    psql (9.1.2)
    Type "help" for help.

    opscode_chef_test=# \d+ users
                                        Table "public.users"
             Column                   |            Type             | Modifiers | Storage  | Description
    ----------------------------------+-----------------------------+-----------+----------+-------------
     id                               | character(32)               | not null  | extended |
     authz_id                         | character(32)               | not null  | extended |
     username                         | text                        | not null  | extended |
     email                            | text                        | not null  | extended |
     pubkey_version                   | integer                     | not null  | plain    |
     public_key                       | text                        |           | extended |
     serialized_object                | text                        |           | extended |
     last_updated_by                  | character(32)               | not null  | extended |
     created_at                       | timestamp without time zone | not null  | plain    |
     updated_at                       | timestamp without time zone | not null  | plain    |
     external_authentication_uid      | text                        |           | extended |
     recovery_authentication_enabled  | boolean                     |           | plain    |
    Indexes:
        "users_pkey" PRIMARY KEY, btree (id)
        "users_authz_id_key" UNIQUE CONSTRAINT, btree (authz_id)
        "users_email_key" UNIQUE CONSTRAINT, btree (email)
        "users_username_key" UNIQUE CONSTRAINT, btree (username)
    Has OIDs: no

    opscode_chef_test=#


