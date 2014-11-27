-module(lrw_prop).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROPMOD, proper).
-define(PROP(A), {timeout, 45, ?_assert(?PROPMOD:quickcheck(A(), [10000]))}).

proper_test_() ->
    {"Run all property-based tests",
     [?PROP(prop_all_ip), ?PROP(prop_all),
      ?PROP(prop_top_ip), ?PROP(prop_top)]}.

prop_all_ip() ->
    ?FORALL({Key,IPs}, {term(), nonempty_list(inet:ip4_address())},
        begin
            Res = lrw:all_ip(Key, IPs),
            Res = lrw:all_ip(Key, lists:reverse(IPs)), % order is okay
            Res--[hd(IPs)] =:= lrw:all_ip(Key, tl(IPs)) % losing nodes is okay
        end).

prop_all() ->
    %% we're using an ip address to include IPv4 and IPv6, but any
    %% term should work. This just speeds up the test.
    ?FORALL({Key,Nodes}, {term(), nonempty_list(inet:ip_address())},
        begin
            Res = lrw:all(Key, Nodes),
            Res = lrw:all(Key, lists:reverse(Nodes)), % order is okay
            Res--[hd(Nodes)] =:= lrw:all(Key, tl(Nodes)) % losing nodes is okay
        end).

prop_top_ip() ->
    ?FORALL({Key,IPs, N}, {term(), nonempty_list(inet:ip4_address()), pos_integer()},
        begin
            All = lrw:all_ip(Key, IPs),
            Top = lrw:top_ip(Key, IPs, N),
            length(Top) =< N % asking for 4 out of 2 yields 2.
            andalso
            lists:prefix(Top, All) % same order
        end).

prop_top() ->
    %% we're using an ip address to include IPv4 and IPv6, but any
    %% term should work. This just speeds up the test.
    ?FORALL({Key,Nodes, N}, {term(), nonempty_list(inet:ip_address()), pos_integer()},
        begin
            All = lrw:all(Key, Nodes),
            Top = lrw:top(Key, Nodes, N),
            length(Top) =< N % asking for 4 out of 2 yields 2.
            andalso
            lists:prefix(Top, All) % same order
        end).

