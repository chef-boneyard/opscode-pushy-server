%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@opscode.com>

%% @copyright Copyright 2012 Chef Software, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
-module(pushy_wm_base).

-export([init/1,
         is_authorized/2,
         malformed_request/2,
         read_forbidden/2,
         write_forbidden/2]).

-include("pushy_wm.hrl").

init([{incarnation_id, IncarnationId}]) ->
    {ok, #config_state{incarnation_id = IncarnationId}}.

malformed_request(Req, State) ->
    GetHeader = get_header_fun(Req),
    try
        chef_authn:validate_headers(GetHeader, ?AUTH_SKEW),
        Req1 = body_not_too_big(Req),
        {false, Req1, State}
    catch
        throw:bad_clock ->
            Msg1 = malformed_request_message(bad_clock, Req, State),
            Req3 = wrq:set_resp_body(jiffy:encode(Msg1), Req),
            {{halt, 401}, Req3, State};
        throw:{bad_headers, Headers} ->
            Msg1 = malformed_request_message({bad_headers, Headers}, Req, State),
            Req3 = wrq:set_resp_body(jiffy:encode(Msg1), Req),
            {{halt, 400}, Req3, State};
        throw:bad_sign_desc ->
            Msg1 = malformed_request_message(bad_sign_desc, Req, State),
            Req3 = wrq:set_resp_body(jiffy:encode(Msg1), Req),
            {{halt, 400}, Req3, State};
        throw:{too_big, Msg} ->
            error_logger:info_msg("json too large (~p)", [Msg]),
            Req3 = wrq:set_resp_body(jiffy:encode({[{<<"error">>, Msg}]}), Req),
            {{halt, 413}, Req3, State};
        throw:Why ->
            Msg = malformed_request_message(Why, Req, State),
            NewReq = wrq:set_resp_body(jiffy:encode(Msg), Req),
            {true, NewReq, State}
    end.

malformed_request_message(bad_clock, Req, _State) ->
    GetHeader = get_header_fun(Req),
    User = case GetHeader(<<"X-Ops-UserId">>) of
               undefined -> <<"">>;
               UID -> UID
           end,
    Msg = iolist_to_binary([<<"Failed to authenticate as ">>, User,
                            <<". Synchronize the clock on your host.">>]),
    {[{<<"error">>, [Msg]}]};
malformed_request_message(bad_sign_desc, _Req, _State) ->
    Msg = <<"Unsupported authentication protocol version">>,
    {[{<<"error">>, [Msg]}]};
malformed_request_message({missing_headers, Missing}, _Req, _State) ->
    Msg = iolist_to_binary([
                            <<"missing required authentication header(s) ">>,
                            bin_str_join(Missing, <<", ">>)]),
    {[{<<"error">>, [Msg]}]};
malformed_request_message({bad_headers, Bad}, _Req, _State) ->
    Msg = iolist_to_binary([
                            <<"bad header(s) ">>,
                            bin_str_join(Bad, <<", ">>)]),
    {[{<<"error">>, [Msg]}]};
malformed_request_message(Reason, _Req, _State) ->
    Msg = iolist_to_binary([<<"unexpected malformed request message: ">>, Reason]),
    {[{<<"error">>, [Msg]}]}.

bin_str_join(L, Sep) ->
    bin_str_join(L, Sep, []).

bin_str_join([H], _Sep, Acc) ->
    lists:reverse([<<"'">>, H, <<"'">>|Acc]);
bin_str_join([H | T], Sep, Acc) ->
    bin_str_join(T, Sep, [Sep, <<"'">>, H, <<"'">> | Acc]).

extract_header(Req, Header) ->
    Name = case is_binary(Header) of
               true -> binary_to_list(Header);
               false -> Header
           end,
    case wrq:get_req_header(string:to_lower(Name), Req) of
        B when is_binary(B) -> B;
        "" -> undefined; %% We want to treat empty header values as missing
        S when is_list(S) -> iolist_to_binary(S);
        undefined -> undefined
    end.

get_header_fun(Req) ->
    fun(H) -> extract_header(Req, H) end.

body_not_too_big(Req) ->
    body_not_too_big(wrq:method(Req), wrq:set_max_recv_body(?MAX_SIZE, Req)).

body_not_too_big(Method, Req) when Method =:= 'POST';
                                   Method =:= 'PUT' ->
    try
        %% Force a read of request body. Webmachine memoizes this in
        %% the process dictionary. Webmachine will read in chunks and
        %% call exit/1 if the body exceeds the max set above. It would
        %% be nice if there was something other than a string to match
        %% against. TODO: patch webmachine.
        wrq:req_body(Req),
        Req
    catch
        exit:"request body too large" ->
            Msg = iolist_to_binary([<<"JSON must be no more than ">>,
                                    integer_to_list(?MAX_SIZE),
                                    <<" bytes.">>]),
            throw({too_big, Msg})
    end;
body_not_too_big(_Method, Req) ->
    Req.

is_authorized(Req, State) ->
    case verify_request_signature(Req, State) of
        {true, Req1, State1} ->
            {true, Req1, State1};
        {false, ReqOther, StateOther} ->
            {"X-Ops-Sign version=\"1.0\" version=\"1.1\"", ReqOther, StateOther};
        {conn_failed, ReqFailure, StateFailure} ->
            {{halt, 502}, ReqFailure, StateFailure};
        {{not_found, org}, ReqNotFound, StateNotFound} ->
            {{halt, 404}, ReqNotFound, StateNotFound};
        {not_associated_with_org, ReqForbid, StateForbid} ->
            {{halt, 403}, ReqForbid, StateForbid}
    end.

%% @doc Perform request signature verification (authenticate)
%%
%% Fetches user or client certificate and uses it verify the signature
%% on the request.  If the request cannot be verified, then the
%% returned `#wm_reqdata{}' record will have a response body
%% explaining why.
verify_request_signature(Req, State) ->
    UserName = list_to_binary(wrq:get_req_header("x-ops-userid", Req)),
    OrgName = list_to_binary(wrq:path_info(organization_id, Req)),
    State1 = State#config_state{organization_guid = pushy_object:fetch_org_id(OrgName),
                                organization_name = OrgName},
    case pushy_principal:fetch_principal(OrgName, UserName) of
        {not_found, not_associated_with_org} ->
            NotFoundMsg = verify_request_message(not_associated_with_org,
                                                 UserName, OrgName),
            Req1 = wrq:set_resp_body(jiffy:encode(NotFoundMsg), Req),
            {not_associated_with_org, Req1, State1};
        {not_found, org} ->
            NotFoundMsg = verify_request_message({not_found, org},
                                                 UserName, OrgName),
            {{not_found, org}, wrq:set_resp_body(jiffy:encode(NotFoundMsg), Req), State1};
        {not_found, What} ->
            NotFoundMsg = verify_request_message({not_found, What},
                                                 UserName, OrgName),
            {false, wrq:set_resp_body(jiffy:encode(NotFoundMsg), Req), State1};
        {conn_failed, Why} ->
            ConnFailedMsg = verify_request_message({conn_failed, Why},
                                                   UserName, OrgName),
            {conn_failed, wrq:set_resp_body(jiffy:encode(ConnFailedMsg), Req), State1};
        #pushy_principal{requestor_key = PublicKey,
                         requestor_type = Type,
                         requestor_id = RequestorId} ->
            DecodedPubKey = chef_authn:extract_public_key(PublicKey),
            Body = body_or_default(Req, <<>>),
            HTTPMethod = method_as_binary(Req),
            Path = iolist_to_binary(wrq:path(Req)),
            GetHeader = get_header_fun(Req),
            case chef_authn:authenticate_user_request(GetHeader, HTTPMethod,
                                                      Path, Body, DecodedPubKey,
                                                      ?AUTH_SKEW) of
                {name, _} ->
                    {true, Req, State1#config_state{requestor = UserName,
                                                    requestor_id = RequestorId,
                                                    requestor_type = Type,
                                                    requestor_key = DecodedPubKey}};
                {no_authn, Reason} ->
                    Msg = verify_request_message(Reason, UserName, OrgName),
                    Json = jiffy:encode(Msg),
                    Req1 = wrq:set_resp_body(Json, Req),
                    {false, Req1, State1}
            end
    end.

body_or_default(Req, Default) ->
    case wrq:req_body(Req) of
        undefined -> Default;
        Body -> Body
    end.

method_as_binary(Req) ->
    iolist_to_binary(atom_to_list(wrq:method(Req))).

write_forbidden(Req, State) ->
    case forbidden(Req, State, "pushy_job_writers", not_found) of
        {not_found, Req1, State1} ->
            forbidden(Req1, State1, "admins", true);
        Result ->
            Result
    end.

read_forbidden(Req, State) ->
    forbidden(Req, State, "pushy_job_readers", false).

forbidden(Req, #config_state{requestor = UserName, requestor_type = Type,
                             organization_name = OrgName} = State, Group, NotFound) ->
    case pushy_check_groups:group_membership(binary_to_list(UserName),
                                             Type,
                                             binary_to_list(OrgName),
                                             Group) of
        group_not_found ->
            {NotFound, Req, State};
        true ->
            {false, Req, State};
        false ->
            Msg = iolist_to_binary([<<"User or client '">>, UserName, <<"' does not ">>,
                                    <<"have access to that action on this server.">>]),
            Req1 = wrq:set_resp_body(jiffy:encode({[{<<"error">>, [Msg]}]}), Req),
            {true, Req1, State}
    end.

verify_request_message({not_found, org}, _User, Org) ->
    Msg = iolist_to_binary([<<"organization '">>, Org, <<"' does not exist.">>]),
    {[{<<"error">>, [Msg]}]};
verify_request_message({not_found, _}, User, _Org) ->
    Msg = iolist_to_binary([<<"Failed to authenticate as '">>, User, <<"'. ">>,
                            <<"Ensure that your node_name and client key ">>,
                            <<"are correct.">>]),
    {[{<<"error">>, [Msg]}]};
verify_request_message({conn_failed, Why}, _User, _Org) ->
    Msg = iolist_to_binary([<<"Failed to connect to erchef to get key: ">>,
                            io_lib:format("~p", [Why])]),
    {[{<<"error">>, [Msg]}]};
verify_request_message(bad_sig, User, _Org) ->
    Msg = iolist_to_binary([<<"Invalid signature for user or client '">>,
                            User,<<"'">>]),
    {[{<<"error">>, [Msg]}]};
verify_request_message(ErrorType, User, Org)  when
      ErrorType =:= not_associated_with_org orelse
      ErrorType =:= unverified_org_membership ->
    Msg = iolist_to_binary([<<"'">>, User,
                            <<"' is not associated with organization '">>,
                            Org, <<"'">>]),
    {[{<<"error">>, [Msg]}]}.
