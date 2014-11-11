%% Library of incoming data validation rules
%% For example, use the following:
%%
%% Source = [1,2,3,4]
%% Source = 123
%% Source = "test@gmail.com"
%% Source = <<"abcde">>
%%
%% Rules =
%% [
%% {length, 4},
%% {length_range, {0,5}},
%% {bit_size, 32},
%% {bit_size_range, {8, 40}},
%% {value_range, {0,100}},
%% {regexp, "^([\\w\\.-]+)@([\\w\\.-]+)$"},
%% {regexp, [<<"bcde">>,<<"cd">>]}
%% ]

-module(validate_wrapper).

-export([start/2]).
-spec start(Source::binary() | list() | integer() | float(), Rules::list()) ->
  {error, Type::atom()} | ok .
start(Source, Rules) ->
  validate:main(Source, Rules).