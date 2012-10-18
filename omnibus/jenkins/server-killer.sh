#!/bin/bash -x

# private chef killing script: nukes OPC

# for backing out centos5/sysvinit-style launching of runsvdir-start
if [ -e "/etc/inittab" ]; then
  sudo egrep -v "/opt/opscode-push-jobs-server/embedded/bin/runsvdir-start" /etc/inittab > /etc/inittab.new && sudo mv /etc/inittab.new /etc/inittab && sudo kill -1 1
fi

if [ -e "/etc/init/opscode-runsvdir.conf" ]; then
  sudo rm /etc/init/opscode-runsvdir.conf
fi

ps ax | egrep 'runsvdir -P /opt/opscode-push-jobs-server/service' | grep -v grep | awk '{ print $1 }' | xargs sudo kill -HUP
sleep 5
ps ax | egrep 'runsvdir -P /opt/opscode-push-jobs-server/service' | grep -v grep | awk '{ print $1 }' | xargs sudo kill -TERM
ps ax | egrep 'svlogd -tt /var/log/opscode-push-jobs-server/' | grep -v grep | awk '{ print $1 }' | xargs sudo kill -TERM
sudo rm -rf /opt/opscode-push-jobs-server
sudo rm -rf /var/opt/opscode-push-jobs-server
sudo rm -rf /var/log/opscode-push-jobs-server
sudo rm -rf /tmp/opt
sudo rm -rf /etc/opscode-push-jobs-server
sleep 5
sudo lsof|grep deleted |awk '{print $2}'|sort|uniq|xargs sudo kill -9

# always succeed
exit 0
