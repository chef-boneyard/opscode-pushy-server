# Chef Push Jobs Server
[![Build Status](https://travis-ci.org/chef/opscode-pushy-server.svg?branch=master)](https://travis-ci.org/chef/opscode-pushy-server)

## Getting Help

We use GitHub issues to track bugs and feature requests. If you need help please post to our Mailing List or join the Chef Community Slack.

 * Chef Community Slack at http://community-slack.chef.io/
 * Chef Mailing List https://discourse.chef.io/

## Building Packages

Follow the instructions in [omnibus/README.md](omnibus/README.md).

## Docker-based Development Environment

Run `make shell` to start a Docker-based development environment. This will
start Postgres, run the database schema migrations, update the rebar
dependencies, and put you into a shell where you can run `make` to compile and
`make test` to run the tests.

## Local Development Environment

* Requires Erlang 18+
* Running instance of postgres 9.4+

### Setup the Schema

In the pushy\_server\_schema directory:

        make setup_schema

Add the following to `config/vars.config`: (Don't commit this change)

        {db_user, "<your local user account>"}.

In the root directory of this repository:

        make test

## Contributing

For information on contributing to this project see <https://github.com/chef/chef/blob/master/CONTRIBUTING.md>

## License

All files in the repository are licensed under the Apache 2.0 license. If any
file is missing the License header it should assume the following is attached;

Copyright (c) 2008-2018 Chef Software, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
