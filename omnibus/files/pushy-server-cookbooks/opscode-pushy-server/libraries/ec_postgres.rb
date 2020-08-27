class EcPostgres
  # Provides a superuser connection to the specified database
  def self.with_connection(node, database = 'template1', opts = {})
    require 'pg'
    password = opts['db_superuser_password'] || PushServer::Secrets.veil.get('postgresql', 'db_superuser_password')
    postgres = node['pushy']['postgresql'].merge(opts)
    connection = PG::Connection.open('user' => postgres['db_superuser'],
                               'host' => postgres['vip'],
                               'password' => password,
                               'port' => postgres['port'],
                               'sslmode' => postgres['sslmode'],
                               'dbname' => database)
    begin
      yield connection
    ensure
      connection.close
    end
  end

  # By default, with_connection will create a superuser connection over tcp to the specified database.
  # This method will create a unix socket connection to a local database instance. This should only be used
  # to the extent required to set configure tcp access and set a password for the superuser.
  def self.with_local_connection(node, database = 'template1')
    require 'pg'
    postgres = node['pushy']['postgresql']
    as_user(postgres['username']) do
      connection = PG::Connection.open('dbname' => database)
      begin
        yield connection
      ensure
        connection.close
      end
    end
  end
  private

  def self.as_user(user)
    # Find the user in the password database.
    u = (user.is_a? Integer) ? Etc.getpwuid(user) : Etc.getpwnam(user)

    old_process_euid = Process.euid
    Process::UID.eid = u.uid
    begin
      yield
    ensure
      Process::UID.eid = old_process_euid
    end
  end
end
