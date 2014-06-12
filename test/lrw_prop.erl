-module(lrw_prop).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROPMOD, proper).
-define(PROP(A), {timeout, 45, ?_assert(?PROPMOD:quickcheck(A(), [10000]))}).

proper_test_() ->
    {"Run all property-based tests",
     [?PROP(prop_all), ?PROP(prop_all_),
      ?PROP(prop_top), ?PROP(prop_top_)]}.

prop_all() ->
    ?FORALL({Key,IPs}, {term(), nonempty_list(inet:ip4_address())},
        begin
            Res = lrw:all(Key, IPs),
            Res = lrw:all(Key, lists:reverse(IPs)), % order is okay
            Res--[hd(IPs)] =:= lrw:all(Key, tl(IPs)) % losing nodes is okay
        end).

prop_all_() ->
    %% we're using an ip address to include IPv4 and IPv6, but any
    %% term should work. This just speeds up the test.
    ?FORALL({Key,Nodes}, {term(), nonempty_list(inet:ip_address())},
        begin
            Res = lrw:all_(Key, Nodes),
            Res = lrw:all_(Key, lists:reverse(Nodes)), % order is okay
            Res--[hd(Nodes)] =:= lrw:all_(Key, tl(Nodes)) % losing nodes is okay
        end).

prop_top() ->
    ?FORALL({Key,IPs, N}, {term(), nonempty_list(inet:ip4_address()), pos_integer()},
        begin
            All = lrw:all(Key, IPs),
            Top = lrw:top(Key, IPs, N),
            length(Top) =< N % asking for 4 out of 2 yields 2.
            andalso
            lists:prefix(Top, All) % same order
        end).

prop_top_() ->
    %% we're using an ip address to include IPv4 and IPv6, but any
    %% term should work. This just speeds up the test.
    ?FORALL({Key,Nodes, N}, {term(), nonempty_list(inet:ip_address()), pos_integer()},
        begin
            All = lrw:all_(Key, Nodes),
            Top = lrw:top_(Key, Nodes, N),
            length(Top) =< N % asking for 4 out of 2 yields 2.
            andalso
            lists:prefix(Top, All) % same order
        end).

