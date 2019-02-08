#!/bin/bash
set -ueo pipefail

channel="${CHANNEL:-unstable}"
product=push-jobs-server
version="${VERSION:-latest}"
dep_channel="${DEP_CHANNEL:-current}"

create_license_guard_file() {
  SERVER_INSTALL_DIR="$1"

  echo "Creating license acceptance guard file"
  sudo mkdir -p "/var/opt/$(basename $SERVER_INSTALL_DIR)"
  sudo touch "/var/opt/$(basename $SERVER_INSTALL_DIR)/.license.accepted"
}

echo "--- Installing $dep_channel chef-server latest"
install-omnibus-product -c "$dep_channel" -P chef-server -v latest

echo "--- Installing $channel $product $version"
package_file="$(install-omnibus-product -c "$channel" -P "$product" -v "$version" | tail -n 1)"

if [[ "$package_file" == *.rpm ]]; then
  check-rpm-signed "$package_file"
fi

echo "--- Testing $channel $product $version"

export PATH="/opt/opscode-push-jobs-server/bin:/opt/opscode-push-jobs-server/embedded/bin:$PATH"
export INSTALL_DIR="/opt/opscode-push-jobs-server"

echo ""
echo ""
echo "============================================================"
echo "Verifying ownership of package files"
echo "============================================================"
echo ""

NONROOT_FILES="$(find "$INSTALL_DIR" ! -uid 0 -print)"
if [[ "$NONROOT_FILES" == "" ]]; then
  echo "Packages files are owned by root.  Continuing verification."
else
  echo "Exiting with an error because the following files are not owned by root:"
  echo "$NONROOT_FILES"
  exit 1
fi

echo ""
echo ""
echo "============================================================"
echo "Reconfiguring $product"
echo "============================================================"
echo ""

sudo mkdir -p /etc/opscode

printf -- "
nginx['ssl_dhparam']='/etc/opscode/dhparam.pem'
insecure_addon_compat false
" | sudo tee /etc/opscode/chef-server.rb

printf -- "-----BEGIN DH PARAMETERS-----
MIIBCAKCAQEAtAvx3pUHBNcK2nD58nPPlKtJzZvrFCyKEn9BSn16/BmFwBhL8rh4
+fkrnLflZ/k9wJjiUkU0DCi+Fy6DUohPHOmmT0BiuwgsDZAFDyTj0PeZKINpbHnQ
EbZENzWo5s5hsb1zVxIMEtTMRrigdHM3FQupFbzOHxonkO0JlocarOJBHGX+Crjp
y/8SReCpC71R+Vl6d4+Dw6GFdL+6k6W558dPfq3UeV8HPWQEaM7/jXDUKJZ0tB6a
1csrekkz3gBFlSjSxececRVn8bm5dTfc86rIWJWeWQVLYdBFT6zi43AvF+nLYKYh
+oVnVrhWgOLYvEKX311d9SaqcdrXVFscYwIBAg==
-----END DH PARAMETERS-----
" | sudo tee /etc/opscode/dhparam.pem

sudo mkdir -p /etc/opscode-push-jobs-server

printf -- "
opscode_pushy_server['heartbeat_interval'] = 1000
" | sudo tee /etc/opscode-push-jobs-server/opscode-push-jobs-server.rb

sudo chef-server-ctl reconfigure
sleep 120

create_license_guard_file /opt/opscode-push-jobs-server || true
sudo opscode-push-jobs-server-ctl reconfigure || true
sleep 120

echo ""
echo ""
echo "============================================================"
echo "Running verification for $product"
echo "============================================================"
echo ""

sudo opscode-push-jobs-server-ctl test -J pedant.xml --all
