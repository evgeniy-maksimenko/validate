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

-spec main(Source::binary() | list() | integer() | float(), Rules::list()) ->
{error, Type::atom()} | ok .
main(Source,Rules) ->
  start(Source, Rules).

-spec start(Source::binary() | list() | integer() | float(), Rules::list()) ->
  {error, Type::atom()} | ok .
start(Source, Rules) ->
  try
    List = [ check_rule(Type, Type, Data, Source) || {Type, Data}<-Rules ],
    isset_error(
      lists:keymember(error,1,List),
      lists:keyfind(error,1,List),
      lists:keymember(match,1,List),
      lists:keyfind(match,1,List))
  catch
    _ : _Reason -> {error, {wrong_format,datatype}}
  end.

-spec isset_error(boolean(), atom(), boolean(), list()) -> atom() | list().
isset_error(true, Listskeyfind, _, _) -> Listskeyfind;
isset_error(false, _Listskeyfind, true, Match) -> Match;
isset_error(false, _Listskeyfind, false, _Match) -> ok.

-spec check_rule(atom(), Type::atom(), Data::binary() | list() | integer() | float(), Source::binary() | list() | integer() | float()) ->
  {error, Type::atom()} | ok .
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

-spec check_source({atom(), atom()}, atom()) -> {error, Type::atom()} | ok .
check_source(nomatch, _Type) -> {error, regexp};
check_source({match, _Regexp}, _Type) -> ok;
check_source(true, _Type) -> ok;
check_source(false, Type) -> {error, Type};
check_source({_Key,_Val}, _Type) -> ok.


