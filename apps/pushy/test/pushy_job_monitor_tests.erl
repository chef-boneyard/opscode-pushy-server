%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
%%

-module(pushy_job_monitor_tests).

-include_lib("eunit/include/eunit.hrl").

-include("pushy_sql.hrl").

-define(ORG_ID, <<"AAAA">>).

mocked_modules() ->
    [pushy_sql, pushy_object, pushy_node_state].

setup() ->
    meck:new(mocked_modules(), []),
    {ok, Pid} = pushy_job_monitor:start_link(),
    unlink(Pid),
    Pid.

cleanup(Pid) ->
    exit(Pid, kill),
    timer:sleep(100),
    meck:validate(mocked_modules()),
    meck:unload().

kill_process_test_() ->
    MyJobId = my_job_id,
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [{"Test we can add and kill a monitored process",
       fun() ->
                %% We expect that when the job is killed it'll
                %% map the JobId to a pushy_object and
                %% update state in the DB
                meck:expect(pushy_sql, fetch_job,
                            fun(J) ->
                                    ?assertEqual(MyJobId, J),
                                    {ok, #pushy_job{id = J} }
                            end),
                meck:expect(pushy_object, update_object,
                            fun(update_job, GotJob, GotJobId) ->
                                    ?assertEqual(MyJobId, GotJobId),
                                    ?assertEqual(crashed, GotJob#pushy_job.status),
                                    {ok, 1}
                            end),

                {ok, Pid} = test_fsm:start_link(),
                unlink(Pid),
                pushy_job_monitor:monitor_job(MyJobId, Pid),
                ?assertEqual(true, pushy_job_monitor:is_monitored(Pid)),
                exit(Pid, kill),
                timer:sleep(10),
                ?assertEqual(false, pushy_job_monitor:is_monitored(Pid))

       end
      },
      {"Test that a crashed process cleans up",
       fun() ->
                %% We expect that when the job crashes it'll
                %% map the JobId to a pushy_object and
                %% update state in the DB
                meck:expect(pushy_sql, fetch_job,
                            fun(J) ->
                                    ?assertEqual(MyJobId, J),
                                    {ok, #pushy_job{id = J} }
                            end),
                meck:expect(pushy_object, update_object,
                            fun(update_job, GotJob, GotJobId) ->
                                    ?assertEqual(MyJobId, GotJobId),
                                    ?assertEqual(crashed, GotJob#pushy_job.status),
                                    {ok, 1}
                            end),
                {ok, Pid} = test_fsm:start_link(),
                unlink(Pid),
                pushy_job_monitor:monitor_job(MyJobId, Pid),
                ?assertEqual(true, pushy_job_monitor:is_monitored(Pid)),
                test_fsm:crash_me(),
                timer:sleep(10),
                ?assertEqual(false, pushy_job_monitor:is_monitored(Pid))
       end
      },
      {"Test that a crashed process with nodes sends them to rehab",
       fun() ->
                meck:expect(pushy_sql, fetch_job,
                            fun(J) ->
                                    ?assertEqual(MyJobId, J),
                                    {ok, #pushy_job{id = J,
                                                    job_nodes = mk_job_nodes(J)} }
                            end),
                meck:expect(pushy_object, update_object,
                            fun(update_job, GotJob, GotJobId) ->
                                    ?assertEqual(MyJobId, GotJobId),
                                    ?assertEqual(crashed, GotJob#pushy_job.status),
                                    {ok, 1}
                            end),
                meck:expect(pushy_node_state, rehab,
                            fun({OrgId, _NodeName}) ->
                                    ?assertEqual(?ORG_ID, OrgId),
                                    ok
                            end),
                {ok, Pid} = test_fsm:start_link(),
                unlink(Pid),
                pushy_job_monitor:monitor_job(MyJobId, Pid),
                ?assertEqual(true, pushy_job_monitor:is_monitored(Pid)),
                test_fsm:crash_me(),
                timer:sleep(10),
                ?assertEqual(false, pushy_job_monitor:is_monitored(Pid)),

                %% Both nodes are sent to rehab
                ?assertEqual(1, meck:num_calls(pushy_node_state, rehab, [{?ORG_ID, <<"Node1">>}])),
                ?assertEqual(1, meck:num_calls(pushy_node_state, rehab, [{?ORG_ID, <<"Node2">>}]))
       end
      },
      {"Test a monitored process that ends gets removed from the monitor",
       fun() ->
                %% We expect that when the job is stopped it'll
                %% signal job_monitor and get removed from the dict
                %% but no mocked methods gets called
                {ok, Pid} = test_fsm:start_link(),
                unlink(Pid),
                pushy_job_monitor:monitor_job(MyJobId, Pid),
                ?assertEqual(true, pushy_job_monitor:is_monitored(Pid)),
                test_fsm:stop_me(),
                %% allow time for job_monitor to get the message
                timer:sleep(10),
                ?assertEqual(false, pushy_job_monitor:is_monitored(Pid)),
                assert_no_calls(mocked_modules())
       end
      }
     ]
    }.

assert_no_calls([]) ->
    ok;
assert_no_calls([Module | Rest]) ->
    ?assertEqual(0, meck:num_calls(Module, '_', '_')),
    assert_no_calls(Rest).

mk_job_nodes(J) ->
    [#pushy_job_node{job_id = J,
                     org_id = ?ORG_ID,
                     node_name = <<"Node1">>,
                     status = running},
     #pushy_job_node{job_id = J,
                     org_id = ?ORG_ID,
                     node_name = <<"Node2">>,
                     status = succeeded}].

