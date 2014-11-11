-module(validate_tests).
-include_lib("eunit/include/eunit.hrl").

main_test() ->
  {error,{wrong_format,datatype}} = validate:main([1,2,3,4],[
    {length, 4},
    {length_range, {0,5}},
    {bit_size, 32},
    {bit_size_range, {8, 40}},
    {value_range, {0,100}},
    {regexp, [<<"bcde">>,<<"cd">>]}
  ]),
  {error,bit_size} = validate:main([1,2,3,4],[
    {length, 4},
    {length_range, {0,5}},
    {bit_size, 32},
    {bit_size_range, {8, 40}},
    {value_range, {0,100}}
  ]
  ),
  {error,value_range} = validate:main([1,2,3,4],[
    {length, 4},
    {length_range, {0,5}},
    {value_range, {0,100}}
  ]
  ),
  ok = validate:main([1,2,3,4],[
    {length, 4},
    {length_range, {0,5}}
  ]
  ),
  {error,length} = validate:main([1,2,3,4, 6],[
    {length, 4},
    {length_range, {0,5}}
  ]
  ),
  {error,length_range} = validate:main([1,2,3,4, 6],[
    {length_range, {0,3}}
  ]
  ),
  {error,length} = validate:main(<<"abcde">>,[
    {length, 4},
    {length_range, {0,5}},
    {bit_size, 32},
    {bit_size_range, {8, 40}},
    {value_range, {0,100}},
    {regexp, [<<"bcde">>,<<"cd">>]}
  ]
  ),
  {error,length_range} = validate:main(<<"abcde">>,[
    {length_range, {0,5}},
    {bit_size, 32},
    {bit_size_range, {8, 40}},
    {value_range, {0,100}},
    {regexp, [<<"bcde">>,<<"cd">>]}
  ]
  ),
  {match,{1,4}} = validate:main(<<"abcde">>,[
    {regexp, [<<"bcde">>,<<"cd">>]}
  ]
  ),
  {error,length_range} = validate:main(123,[
    {length_range, {0,5}},
    {bit_size, 32},
    {bit_size_range, {8, 40}},
    {value_range, {0,100}}
  ]
  ),
  {error,value_range} =  validate:main(123,[
    {value_range, {0,100}}
  ]
  ),
  ok = validate:main(12,[
    {value_range, {0,100}}
  ]
  ),
  {match,["192.168.1.241"]} = validate:main("http://192.168.1.241/mod/fun?arg",[
    {regexp, "(\\d{1,3}\\.){3}\\d{1,3}"}
  ]
  ).

