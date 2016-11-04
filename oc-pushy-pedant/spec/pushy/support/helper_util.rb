require 'pushy_client'
require 'json'
require 'pp'

shared_context "helper_util" do
  def heartbeat_interval
    filename = "/etc/opscode-push-jobs-server/opscode-push-jobs-server-running.json"
    if File.exists?(filename)
      file = File.read(filename)
      data = JSON.parse(file)
      interval_millis = data['pushy']['opscode-pushy-server']['heartbeat_interval']
      (interval_millis.to_i)/1000.0 #return heartbeat interval in seconds (as a float)
    else
      pp "Please run opscode-push-jobs-server-ctl reconfigure before running tests."
    end
  end
end
