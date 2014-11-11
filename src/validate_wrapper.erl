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

-type source()    :: proplists().
-type proplists() :: [kvtuple()].
-type kvtuple()   :: {key(), value()}.
-type key()       :: binary().
-type value()     :: binary().

-type rules()     :: ruleslist().
-type ruleslist() :: [pvtuple()].
-type pvtuple()   :: {property(), amount()}.
-type property()  :: length | length_range | bit_size | bit_size_range | value_range | regexp.
-type amount()    :: binary() | list() | integer() | float().

-type error()           :: errortuple_kv() | ok.
-type errortuple_kv()   :: {error, property() | kv_error_tuple()}.
-type kv_error_tuple()  :: {key_error(), value_error()}.
-type key_error()       :: wrong_format.
-type value_error()     :: datatype.

-spec start(Source::source(), Rules::rules()) -> error().
start(Source, Rules) ->
  validate:main(Source, Rules).