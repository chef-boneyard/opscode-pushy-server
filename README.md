# Overview

"pushy" is the internal nickname for the new push job feature. In this repo you
will find:

* A org-mode file describing the overall design
* A spec compliant agent suitable for production deployment
* A spec compliant server suitable for inclusion in a production OPC installation.

## Building Opscode Pushy Server Package

* Virtualbox
* Vagrant
* Chefdk

Follow the instructions in `omnibus/README.md`.

## Compiling and Running Tests Locally

* Requires Erlang 18+
* Running instance of postgres 9.4+

### Setup the Schema

In the pushy\_server\_schema directory:

        make setup_schema

Add the following to `config/vars.config`: (Don't commit this change)

        {db_user, "<your local user account>"}.

In the root directory of this repository:

        make test

License
=======

Pushy - The push jobs component for chef

|                      |                                          |
|:---------------------|:-----------------------------------------|
| **Copyright:**       | Copyright (c) 2008-2014 Chef Software, Inc.
| **License:**         | Apache License, Version 2.0

All files in the repository are licensed under the Apache 2.0 license. If any
file is missing the License header it should assume the following is attached;

Copyright 2014 Chef Software Inc

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
