FROM erlang:18.2
RUN apt-get update
RUN apt-get install -y mg nano postgresql-9.4 sqitch sudo vim
# We have to explicitly set the LD path for this lib -
# when it's built in a container, the rpath is not correctly set
# on the binary.

# Even though build-time output of erlzmq says:
#     Building with ZEROMQ_PREFIX=/srv/_build/default/lib/erlzmq/deps/local
# And erlzmq_drv is built with the option
#  {"linux", "DRV_LDFLAGS", "-Wl,-rpath=${ZEROMQ_PREFIX}/lib $DRV_LDFLAGS"},
# A check of the final rpath as set in the container-built binary shows:

# root@5ad413cc8d5c:/srv# objdump -x  _build/default/lib/erlzmq/priv/erlzmq_drv.so | grep PATH[
#  RPATH                /lib:/srv/deps/local/lib

#
# So the below works for now in conjunction with 'ldconfig' - but it would
# be nice to come back and hunt it down...
RUN echo /srv/_build/default/lib/erlzmq/deps/local/lib > /etc/ld.so.conf.d/erlzmq_drv.conf

# Use /srv so that we don't have to deal with .bash_history and other dotfiles getting
# created as root in the project directory
WORKDIR /srv
CMD /etc/init.d/postgresql start && \
    (cd /srv/pushy-server-schema && sudo -u postgres make setup_dev) && \
    ldconfig && \
    make install && \
    bash
