FROM erlang:18.2
RUN apt-get update
RUN apt-get install -y mg nano postgresql-9.4 sqitch sudo vim
WORKDIR /root
CMD /etc/init.d/postgresql start && (cd /root/pushy-server-schema && sudo -u postgres make setup_schema) && make install && bash
