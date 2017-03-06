#
# Copyright:: Copyright (c) 2017 Chef Software, Inc
# License:: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
require 'singleton'
require 'veil'

module PushServer
  class Secrets
    include Singleton
    SECRETS_FILE = "/etc/opscode/private-chef-secrets.json"
    OLD_SECRETS_FILE = "/etc/opscode-push-jobs-server/opscode-push-jobs-server-secrets.json"

    PUSH_PRIV_PATH = "/etc/opscode-push-jobs-server/pushy_priv.pem"
    PUSH_PUB_PATH = "/etc/opscode-push-jobs-server/pushy_pub.pem"
    PIVOTAL_PATH = "/etc/opscode/pivotal.pem"

    attr_accessor :user

    def self.bootstrap(node)
      instance.bootstrap(node)
    end

    def self.veil
      instance.veil
    end

    def self.get(name)
      instance.get(name)
    end

    def bootstrap(node)
      @user = node.read('private_chef', 'user', 'username')
      unless @user
        Chef::Log.warn("Could not read Chef server username, using 'opscode'")
        @user = 'opscode'
      end

      migrate_to_veil
      migrate_pivotal_key
      generate_passwords
      create_keys
    end

    def get(name)
      veil.get("push-jobs-server", name)
    end

    def veil
      @veil ||= Veil::CredentialCollection::ChefSecretsFile.from_file(SECRETS_FILE, user: user)
    end

    private

    def add(name, opts={})
      veil.add("push-jobs-server", name, opts)
    end

    def exists?(name)
      veil.exist?("push-jobs-server", name)
    end

    def add_key(group, name, value)
      veil.add(group, name, value: value, frozen: true)
    end

    def migrate_to_veil
      do_migration if migration_required?
    end

    def migrate_pivotal_key
      # TODO(ssd) 2017-03-01: Ensure this matches the PR that lands in
      # chef-server. Also, since we are probably going to have a hard
      # incompatibility break somewhere, we might not even want to do
      # this
      if veil.exist?("chef-server", "superuser_key")
        Chef::Log.debug("Pivotal key already exists in secrets store.")
      elsif ::File.exists?(PIVOTAL_PATH)
        Chef::Log.debug("Pivotal key does not exist in secrets store, migrating it to the secrets store from #{PIVOTAL_PATH}")
        add_key("chef-server", "superuser_key", ::File.read(PIVOTAL_PATH))
        veil.save
      else
        Chef::Log.error("Could not find pivotal key in secrets store or on disk!")
      end
    end

    def generate_passwords
      add('sql_password', length: 50)
      add('sql_ro_password', length: 50)
      veil.save
    end

    def create_keys
      pushy_key = if exists?("pushy_priv_key")
                    OpenSSL::PKey::RSA.new(get("pushy_priv_key"))
                  else
                    OpenSSL::PKey::RSA.generate(2048)
                  end

      add_key("push-jobs-server", "pushy_priv_key", pushy_key.to_pem.to_s)
      add_key("push-jobs-server", "pushy_pub_key", pushy_key.public_key.to_s)
      veil.save
    end

    def do_migration
      old_secrets = Chef::JSONCompat.from_json(::File.read(OLD_SECRETS_FILE))
      old_secrets.each do |group_name, group|
        group.each do |secret_name, value|
          veil.add("push-jobs-server", secret_name, value: value)
        end
      end

      if ::File.exists?(PUSH_PRIV_PATH)
        add_key("push-jobs-server", "pushy_priv_key", ::File.read(PUSH_PRIV_PATH))
      else
        Chef::Log.debug("#{PUSH_PRIV_PATH} does not exist, not migrating")
      end

      if ::File.exists?(PUSH_PUB_PATH)
        add_key("push-jobs-server", "pushy_pub_key", ::File.read(PUSH_PUB_PATH))
      else
        Chef::Log.debug("#{PUSH_PUB_PATH} does not exist, not migrating")
      end

      veil.save

      [PUSH_PRIV_PATH, PUSH_PUB_PATH, OLD_SECRETS_FILE].each do |path|
        Chef::Log.debug("Removing #{path}")
        FileUtils.rm_f(path)
      end
    end

    def migration_required?
      ::File.exist?(OLD_SECRETS_FILE)
    end
  end
end
