%%% @doc Prioritize a list of IPs for a specific key in increasing order
%%% for their weight. The original algorithm in
%%% http://www.eecs.umich.edu/techreports/cse/96/CSE-TR-316-96.pdf goes for the
%%% highest weight available, but describes that no loss of generality will happen
%%% going with LRW or HRW (see page 13 of the technical report). This module
%%% implements LRW specifically.
%%%
%%% Do note that this implements the algorithm as mentioned in the paper, and as
%%% such does not support IPv6 at this time.
%%% @end
-module(lrw).
-export([all/2, top/3,
         all_ip/2, top_ip/3]).
-export([all/3, top/4]).
-define(MOD, 2147483648). % 1 bsl 31

-type hashfun() :: fun((Key::term(), Node::term()) -> number()).
-export_type([hashfun/0]).

%% @doc Returns the given set of nodes sorted in increasing order of
%% their weight for a given key. The weights aren't included in the returned
%% results.
-spec all(Key :: term(), [Node, ...]) -> [Node, ...] when
    Node :: term().
all(Key, Nodes) ->
    Mod = 1 bsl 32,
    all(Key, Nodes, fun(K, Node) -> erlang:phash2({K,Node}, Mod) end).

%% @doc Like `all/2' but for IPs specifically; with an optimized hash.
-spec all_ip(Key :: term(), [NodeIP, ...]) -> [NodeIP, ...] when
    NodeIP :: inet:ip4_address().
all_ip(Key, NodeIPs) ->
    all(Key, NodeIPs, fun(K, NodeIP) -> wrand2(K, to_num_ip(NodeIP)) end).


%% @doc Returns the given set of nodes sorted in increasing order of their
%% weight for a given key. The weights aren't included in the returned results.
%% The third argument must be a function that accepts and returns arbitrary
%% hashes.
-spec all(Key :: term(), [Node, ...], hashfun()) -> [Node, ...] when
    Node :: term().
all(Key, Nodes, Hash) ->
    Weighted = [{Hash(Key, Node), Node} || Node <- Nodes],
    [Node || {_, Node} <- lists:sort(Weighted)].

%% @doc Only keep the `N' top entries, compared to `all/2'. Note that
%% this call is unoptimized and just picks a sublist of the `all/2' algorithm.
-spec top(Key :: term(), [Node, ...], Len :: pos_integer()) -> [Node, ...] when
    Node :: term().
top(Key, Nodes, Len) ->
    lists:sublist(all(Key, Nodes), 1, Len).

%% @doc Like `top/3' but optimized with a special hash for IPs.
-spec top_ip(Key :: term(), [NodeIP, ...], Len :: pos_integer()) -> [NodeIP, ...] when
    NodeIP :: inet:ip4_address().
top_ip(Key, NodeIPs, Len) ->
    lists:sublist(all_ip(Key, NodeIPs), 1, Len).

%% @doc Only keep the `N' top entries, compared to `all/3'. Note that
%% this call is unoptimized and just picks a sublist of the `all/3' algorithm.
%% The fourth argument must be a function that accepts and returns arbitrary
%% hashes.
-spec top(Key :: term(), [Node, ...], Len :: pos_integer(), hashfun()) ->
    [Node, ...] when
    Node :: term().
top(Key, Nodes, Len, Hash) ->
    lists:sublist(all(Key, Nodes, Hash), 1, Len).

%% @private Convert an IPv4 inet address of the form `{A,B,C,D}' to a
%% 32 bit integer to be used in the hashing function.
-spec to_num_ip(inet:ip4_address()) -> non_neg_integer().
to_num_ip({A,B,C,D}) ->
    <<X:32>> = <<A,B,C,D>>,
    X.

%% @private Hashing function as recommended on p.21 of
%% http://www.eecs.umich.edu/techreports/cse/96/CSE-TR-316-96.pdf
-spec wrand2(term(), non_neg_integer()) -> non_neg_integer().
wrand2(K, NodeIP) ->
    Digest = erlang:phash2(K, ?MOD), % should lead to a 2^31 digest
    (1103515245 * ((1103515245 * Digest + 12345) bxor NodeIP)+12345) rem ?MOD.

