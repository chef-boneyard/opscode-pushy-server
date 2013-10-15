name "pushy-server-schema"
version "1.0.0"

# To install, we need sqitch, but we can use the one already installed
# for Enterprise Chef

source :git => "git@github.com:opscode/pushy-server-schema.git"

build do
  command "mkdir -p #{install_dir}/embedded/service/#{name}"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/#{name}/"
end
