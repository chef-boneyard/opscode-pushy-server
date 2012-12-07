shared_context "authorization_groups_util" do
  def setup_group(group_name, users, clients, subgroups)
    post(api_url("/groups"), superuser,
         :payload => { "groupname" => group_name }) do |response|
      response.should look_like({
                                  :status => 201
                                })

    end
    put(api_url("/groups/#{group_name}"), superuser,
        :payload => { "groupname" => group_name, "actors" => { "users" => users,
            "clients" => clients, "groups" => subgroups } } ) do |response|
      response.should look_like({
                                  :status => 200
                                })
    end
  end
end
