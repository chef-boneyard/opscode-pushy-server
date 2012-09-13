# FIRST TIME DATABASE CREATION #

IF you installed opscode-dev-vm from a non-pushy opscode-dev-vm, you will need
to create the database (skip this step if you've done it already):

      sudo -u opscode-pgsql /opt/opscode/embedded/bin/createdb -T template0 -E UTF-8 opscode_pushy
      sudo -u opscode-pgsql /opt/opscode/embedded/bin/psql -d 'opscode_pushy' -c "GRANT ALL PRIVILEGES ON DATABASE opscode_pushy TO opscode_chef"
      sudo -u opscode-pgsql /opt/opscode/embedded/bin/psql -d 'opscode_pushy' -c "GRANT ALL PRIVILEGES ON DATABASE opscode_pushy TO opscode_chef_ro"

# RUNNING MIGRATIONS #

From /opt/opscode/embedded/service/opscode-pushy/db:

      sudo -u opscode-pgsql PATH=/opt/opscode/embedded/bin:$PATH /opt/opscode/embedded/bin/bundle exec /opt/opscode/embedded/bin/rake pg:remigrate

Verify everything migrated correctly:

      psql opscode_pushy opscode-pgsql -c "\dt"
                    List of relations
       Schema |    Name     | Type  |     Owner
      --------+-------------+-------+---------------
       public | job_nodes   | table | opscode-pgsql
       public | jobs        | table | opscode-pgsql
       public | node_status | table | opscode-pgsql
       public | schema_info | table | opscode-pgsql
      (4 rows)

Read moar: <http://sequel.rubyforge.org/rdoc/files/doc/migration_rdoc.html>

# Modifying Schema #

Read here first:
https://wiki.corp.opscode.com/display/CORP/Chef+SQL+Schema
