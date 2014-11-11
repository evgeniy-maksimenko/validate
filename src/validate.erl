%% Library of incoming data validation rules
%% The following pattern characters are recognized:
%%
%% Source
%% ----------------------------------------------------
%% binary | list | integer | float
%%
%% Rule
%% ----------------------------------------------------
%% length | length_range | bit_size | bit_size_range | value_range | regexp
%%

-module(validate).
-export([main/2, start/2, check_rule/4, check_source/2]).

-type source()              :: proplists().
-type proplists()           :: [kvtuple()].
-type kvtuple()             :: {key(), value()}.
-type key()                 :: binary().
-type value()               :: binary().

-type rules()               :: ruleslist().
-type ruleslist()           :: [pvtuple()].
-type pvtuple()             :: {property(), amount()}.
-type property()            :: length | length_range | bit_size | bit_size_range | value_range | regexp.
-type amount()              :: binary() | list() | integer() | float().

-type error()               :: errortuple_kv() | ok.
-type errortuple_kv()       :: {error, property() | kv_error_tuple()}.
-type kv_error_tuple()      :: {key_error(), value_error()}.
-type key_error()           :: wrong_format.
-type value_error()         :: datatype.

-type error_kv()            :: errortuple() | ok.
-type errortuple()          :: {error, property()}.

-type data()                :: list() | integer() | tuple().

-type ck_source_property()  :: property().
-type ck_source_tuple()     :: {ck_source_key(), ck_source_value()} |  nomatch | boolean().
-type ck_source_key()       :: match | integer().
-type ck_source_value()     :: property() | integer() | tuple().

-define(LISTS_KEY_NUMBER, 1).

-spec main(Source::source(), Rules::rules()) -> error().
main(Source,Rules) ->
  start(Source, Rules).

-spec start(Source::source(), Rules::rules()) -> error().
start(Source, Rules) ->
  try
    List = [ check_rule(Type, Type, Data, Source) || {Type, Data}<-Rules ],
    isset_error(
      lists:keymember(error,?LISTS_KEY_NUMBER,List),
      lists:keyfind(error,  ?LISTS_KEY_NUMBER,List),
      lists:keymember(match,?LISTS_KEY_NUMBER,List),
      lists:keyfind(match,  ?LISTS_KEY_NUMBER,List))
  catch
    _ : _Reason -> {error, {wrong_format,datatype}}
  end.

-spec isset_error(boolean(),Listskeyfind::tuple(),boolean(),Match::tuple()) -> tuple() | ok.
isset_error(true, Listskeyfind, _, _) -> Listskeyfind;
isset_error(false, _, true, Match) -> Match;
isset_error(false, _, false, _) -> ok.

-spec check_rule(property(), Type::property(), Data::data(), Source::amount()) -> error_kv().
check_rule(length, Type, Data, Source)
  when (is_integer(Data) and is_list(Source)) ->
  check_source(length(Source) == Data, Type);
check_rule(length_range, Type, Data, Source)
  when (is_tuple(Data) and is_list(Source)) ->
  {Min,Max} = Data,
  check_source(((length(Source) < Max) and (length(Source) > Min)), Type);
check_rule(bit_size, Type, Data, Source)
  when (is_integer(Data) and is_binary(Source)) ->
  check_source(bit_size(Source) == Data, Type);
check_rule(bit_size_range, Type, Data, Source)
  when (is_tuple(Data) and is_binary(Source)) ->
  {Min,Max} = Data,
  check_source(((bit_size(Source) < Max) and (bit_size(Source) > Min)), Type);
check_rule(value_range, Type, Data, Source)
  when (is_tuple(Data) and (is_integer(Source) or is_float(Source))) ->
  {Min,Max} = Data,
  check_source(((Source < Max) and (Source > Min)), Type);
check_rule(regexp, Type, Data, Source)
  when (is_list(Data) and is_list(Source)) ->
  check_source(re:run(Source, Data, [{capture,all_but_first,list}]), Type);
check_rule(regexp, Type, Data, Source)
  when (is_list(Data) and is_binary(Source)) ->
  check_source(binary:match(Source, Data), Type);
check_rule(Type, _Type, _Data, _Source) -> {error, Type}.

-spec check_source(ck_source_tuple(),ck_source_property()) -> error_kv().
check_source(nomatch, _Type)            -> {error, regexp};
check_source({match,  _Regexp}, _Type)  -> ok;
check_source(true,    _Type)            -> ok;
check_source(false,   Type)             -> {error, Type};
check_source({_Key,   _Val}, _Type)     -> ok.


