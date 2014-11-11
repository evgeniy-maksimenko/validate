#Validate

This library contains useful functions to use validate.

**Author**: Evgenij Maksimenko (evgenij.maksimenko.01@privatbank.ua)

Examples
--------

validate_wrapper.erl:

%% Wrapper validate which call validate functions

validate.erl:

.. code_block:: erlang

    > validate:main([1,2,3,4],[
         {length, 4},
         {length_range, {0,5}}
       ]
       ).
    > ok
    
    > validate:main(<<"abcde">>,[
         {regexp, [<<"bcde">>,<<"cd">>]}
       ]
       ).
    > ok
    
    > validate:main(12,[
          {value_range, {0,100}}
        ]
        ).
    > ok
    
    > validate:main("http://192.168.1.241/mod/fun?arg",[
         {regexp, "(\\d{1,3}\\.){3}\\d{1,3}"}
       ]
       ).
    > ok

