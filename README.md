# Rendezvous or Lowest Random Weight (LRW) hashing #

This is an Erlang implementation of LRW (equivalent to HRW), an algorithm that allows
clients to achieve distributed agreement on which site (or proxy) a given
object is to be placed in. It accomplishes the same goal as consistent hashing,
using an entirely different method.

This hashing mechanism allows to consistently hash to specific IP addresses, without
needing to do bucketing at first, or ever keeping a fixed or ever-increasing amount
of entries, while guaranteeing as little key redistribution as possible.

- [http://en.wikipedia.org/wiki/Rendezvous\_hashing](http://en.wikipedia.org/wiki/Rendezvous_hashing)
- ["A Name-Based Mapping Scheme for Rendezvous" (PDF)](http://www.eecs.umich.edu/techreports/cse/96/CSE-TR-316-96.pdf)

## Interface ##

The `all/2` function gives an ordering of all nodes:

    1> lrw:all(my_item, [{127,0,0,1},{255,0,0,1},{198,2,1,2},{192,198,2,1}]).
    [{192,198,2,1},{127,0,0,1},{255,0,0,1},{198,2,1,2}]
    2> lrw:all(my_item, [{127,0,0,1},{198,2,1,2},{192,198,2,1},{10,10,100,10}]).
    [{192,198,2,1},{127,0,0,1},{198,2,1,2},{10,10,100,10}]
    3> lrw:all(my_item, [{127,0,0,1},{192,198,2,1}]).
    [{192,198,2,1},{127,0,0,1}]
    4> lrw:all(my_item, [{127,0,0,1},{192,18,211,12}, {23,66,77,88}, {252,11,11,11}]).
    [{252,11,11,11},{127,0,0,1},{23,66,77,88},{192,18,211,12}]
    5> lrw:all("my other item", [{127,0,0,1},{192,18,211,12}, {23,66,77,88}, {252,11,11,11}]).
    [{23,66,77,88},{127,0,0,1},{252,11,11,11},{192,18,211,12}]
    6> lrw:all(<<"my other item">>, [{127,0,0,1},{192,18,211,12}, {23,66,77,88}, {252,11,11,11}]).
    [{252,11,11,11},{23,66,77,88},{127,0,0,1},{192,18,211,12}]

The `top/3` functions returns a subset. It is using `all/2` in its implementation:

    1> lrw:top(12123, [{127,0,0,1},{255,0,0,1},{198,2,1,2},{192,198,2,1}], 2).
    [{192,198,2,1},{127,0,0,1}]

The list of IPs must be IPv4s.

## Building ##

Pullit in your projects. Compile standalone using rebar (you should have this
installed globally on your node already, it's been in use for years, come on!):

    $ rebar compile

## Running Tests ##

Verify for type errors and discrepancies:

    $ dialyzer --src src/*.erl

Then run the property-based tests (10,000 each) with:

    $ rebar get-deps compile eunit --config rebar.tests.config


