%% Library of incoming data validation rules
%% For example, use the following:
%%
%% Source = [1,2,3,4]
%% Source = 123
%% Source = "http://192.168.1.241/mod/fun?arg"
%% Source = <<"abcde">>
%%
%% Rules =
%% [
%% {length, 4},
%% {length_range, {0,5}},
%% {bit_size, 32},
%% {bit_size_range, {8, 40}},
%% {value_range, {0,100}},
%% {regexp, "(\\d{1,3}\\.){3}\\d{1,3}"},
%% {regexp, [<<"bcde">>,<<"cd">>]}
%% ]

-module(validate_wrapper).

-export([main/0, start/2]).

-spec main() -> {match, Regexp::tuple()} | {error, Type::atom()} | ok | {match , {Key::integer(),Val::integer()}}.
main() ->
  Source = [1,2,3,4],
  Rules =
    [
      {length, 4},
      {length_range, {0,5}}
%%       {bit_size, 32},
%%       {bit_size_range, {8, 40}},
%%       {value_range, {0,100}}
%%       {regexp, "(\\d{1,3}\\.){3}\\d{1,3}"},
%%       {regexp, [<<"bcde">>,<<"cd">>]}
    ],
  validate:main(Source, Rules).

-spec start(Source::binary() | list() | integer() | float(), Rules::list()) ->
  {match, Regexp::tuple()} | {error, Type::atom()} | ok | {match , {Key::integer(),Val::integer()}}.
start(Source, Rules) ->
  validate:main(Source, Rules).