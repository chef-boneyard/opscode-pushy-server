include_recipe 'hurry-up-and-test::set_non_nat_vbox_ip'

Chef::Log.warn("Node fqdn: #{node['fqdn']}")
Chef::Log.warn("Node ipaddress: #{node['ipaddress']}")

chef_ingredient 'chef-server' do
  config <<-EOS
api_fqdn '#{node['fqdn']}'
EOS
end

ingredient_config 'chef-server' do
  notifies :reconfigure, 'chef_ingredient[chef-server]', :immediately
end

chef_ingredient 'manage' do
  accept_license true
  action :install
end

ingredient_config 'manage' do
  notifies :reconfigure, 'chef_ingredient[manage]'
end

chef_ingredient 'push-jobs-server' do
  channel :current
  accept_license true
  action :install
end

ingredient_config 'push-jobs-server' do
  notifies :reconfigure, 'chef_ingredient[push-jobs-server]'
end

execute 'create admin' do
  command <<-EOF.gsub(/\s+/, ' ').strip!
    chef-server-ctl user-create
      admin
      Adam Admin
      cheffio@chef.io
      none11
      --filename #{File.join('/srv', 'admin.pem')}
  EOF
  not_if 'chef-server-ctl user-show admin'
  notifies :run, 'execute[create org]', :immediately
end

execute 'create org' do
  command <<-EOF.gsub(/\s+/, ' ').strip!
    chef-server-ctl org-create
    admin-org
    'Delightful Development Organization'
    --association_user admin
    --filename #{File.join('/srv', 'admin-validator.pem')}
  EOF
  action :nothing
end

template "/srv/knife.rb" do
   source "knife.rb.erb"
   variables(server_url: node['ipaddress'])
   action :create
end
