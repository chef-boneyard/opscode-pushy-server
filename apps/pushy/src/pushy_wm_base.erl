%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@opscode.com>
%% @copyright Copyright 2012 Opscode Inc.

-module(pushy_wm_base).

-export([init/1,
         is_authorized/2,
         malformed_request/2]).

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
        throw:bad_headers ->
            Msg1 = malformed_request_message(bad_headers, Req, State),
            Req3 = wrq:set_resp_body(jiffy:encode(Msg1), Req),
            {{halt, 401}, Req3, State};
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

get_header_fun(Req) ->
    GetHeader = fun(H) ->
                        Name = case is_binary(H) of
                                   true -> binary_to_list(H);
                                   false -> H
                               end,
                        case wrq:get_req_header(string:to_lower(Name), Req) of
                            B when is_binary(B) -> B;
                            S when is_list(S) -> iolist_to_binary(S);
                            undefined -> undefined
                        end
                end,
    GetHeader.

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
            {"X-Ops-Sign version=\"1.0\" version=\"1.1\"", ReqOther, StateOther}
    end.

%% @doc Perform request signature verification (authenticate)
%%
%% Fetches user or client certificate and uses it verify the signature
%% on the request.  If the request cannot be verified, then the
%% returned `#wm_reqdata{}' record will have a response body
%% explaining why.
verify_request_signature(Req, State) ->
    UserName = wrq:get_req_header("x-ops-userid", Req),
    OrgName = list_to_binary(wrq:path_info(organization_id, Req)),
    State1 = State#config_state{organization_guid = pushy_object:fetch_org_id(OrgName),
                                organization_name = OrgName},
    case get_public_key(OrgName, UserName) of
        {not_found, What} ->
            NotFoundMsg = verify_request_message({not_found, What},
                                                 UserName, OrgName),
            {false, wrq:set_resp_body(jiffy:encode(NotFoundMsg), Req), State1};
        PublicKey ->
            Body = body_or_default(Req, <<>>),
            HTTPMethod = method_as_binary(Req),
            Path = iolist_to_binary(wrq:path(Req)),
            GetHeader = get_header_fun(Req),
            case chef_authn:authenticate_user_request(GetHeader, HTTPMethod,
                                                      Path, Body, PublicKey,
                                                      ?AUTH_SKEW) of
                {name, _} ->
                    {true, Req, State1};
                {no_authn, Reason} ->
                    Msg = verify_request_message(Reason, UserName, OrgName),
                    Json = jiffy:encode(Msg),
                    Req1 = wrq:set_resp_body(Json, Req),
                    {false, Req1, State1}
            end
    end.

get_public_key(OrgName, UserName) ->
    pushy_public_key:fetch_public_key(OrgName, UserName).

body_or_default(Req, Default) ->
    case wrq:req_body(Req) of
        undefined -> Default;
        Body -> Body
    end.

method_as_binary(Req) ->
    iolist_to_binary(atom_to_list(wrq:method(Req))).

verify_request_message({not_found, org}, _User, Org) ->
    Msg = iolist_to_binary([<<"organization '">>, Org, <<"' does not exist.">>]),
    {[{<<"error">>, [Msg]}]};
verify_request_message({not_found, _}, User, _Org) ->
    Msg = iolist_to_binary([<<"Failed to authenticate as '">>, User, <<"'. ">>,
                            <<"Ensure that your node_name and client key ">>,
                            <<"are correct.">>]),
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
