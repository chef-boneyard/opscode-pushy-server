# push-setup

Sets up push-server and push-client cluster via test-kitchen to help make manual testing a little quicker.

To use this you need Vagrant >= 1.8.1 and a recent ChefDK.

From this directory:

    bundle install

    ./push-setup

When setup completes, verify that the push cluster is working:

    $ knife node status client
    client  available

    $ knife job start chef-client client
    Started.  Job ID: 29ec910c2a3f040aeebb07c398d93d9e
    .Running (1/1 in progress) ...
    ...Complete.
    command:     chef-client
    created_at:  Wed, 11 May 2016 15:14:35 GMT
    env:
    id:          29ec910c2a3f040aeebb07c398d93d9e
    nodes:
      succeeded: client
    run_timeout: 3600
    status:      complete
    updated_at:  Wed, 11 May 2016 15:14:39 GMT

## TODO

- Allow the platforms to be configurable (currently hardcoded to ubuntu-14.04 for server and client)
- Allow the push-client and push-server versions to be configurable (currently hardcoded to :current)
