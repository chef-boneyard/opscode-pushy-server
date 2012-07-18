%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @copyright 2011-2012 Opscode Inc.

-module(ripped_from_chef_rest).

%% Helpers for webmachine callbacks
-export([set_uri_of_created_resource/1]).

-include_lib("webmachine/include/webmachine.hrl").

%% TODO The below are MERCILESSLY STOLEN FROM chef_rest.  Get this in a common place, yo.
%% @doc Sets the JSON body of a response and it's Location header to
%% point to the URI of a newly-created resource.
%%
%% The body will be of the form
%%
%%     {"uri":"http://foo.com/newresource"}
%%
%% Returns the updated request.
set_uri_of_created_resource(Req) ->
  set_uri_of_created_resource(full_uri(Req), Req).
set_uri_of_created_resource(Uri, Req) when is_list(Uri) ->
    set_uri_of_created_resource(list_to_binary(Uri), Req);
set_uri_of_created_resource(Uri, Req0) when is_binary(Uri) ->
    %% Uri needs to be a binary for encoding to JSON, but a string for the header value
    Req = set_json_body(Req0, {[{<<"uri">>, Uri}]}),
    wrq:set_resp_header("Location", binary_to_list(Uri), Req).

%% @doc Converts the given Ejson-encoded data to a JSON string and
%% sets it as the request body, returning the updated request.
%% @end
set_json_body(Req, EjsonData) ->
    Json = ejson:encode(EjsonData),
    wrq:set_resp_body(Json, Req).

-spec base_uri(#wm_reqdata{}) -> string().
%% @doc Returns the base URI for the server as called by the client as a string.
base_uri(Req) ->
    Scheme = scheme(Req),
    Host = string:join(lists:reverse(wrq:host_tokens(Req)), "."),
    PortString = port_string(wrq:port(Req)),
    Scheme ++ "://" ++ Host ++ PortString.

full_uri(Req) ->
    base_uri(Req) ++ wrq:disp_path(Req).

scheme(Req) ->
    case wrq:get_req_header("x-forwarded-proto", Req) of
        undefined ->
            case wrq:scheme(Req) of
                https -> "https";
                http -> "http";
                P -> erlang:atom_to_list(P)
            end;
        Proto -> Proto
    end.

%% So this is kind of gross and will prevent correct port info if you run https on port 80
%% or http on port 443; otherwise it should work. The problem is two-fold, first webmachine
%% ignores scheme information when parsing the host header and so always sets the port to 80
%% if no port is present in the host header. But in a load-balanced situation, the scheme
%% from webmachine may not reflect what is in use at the load balancer. A simple compromise
%% is to treat both 80 and 443 as default and only include a port string if the port differs
%% from those.
port_string(Default) when Default =:= 80; Default =:= 443 ->
    "";
port_string(Port) ->
    [$:|erlang:integer_to_list(Port)].
