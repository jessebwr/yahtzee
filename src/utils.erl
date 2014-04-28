-module(utils).

-export([nextPow2/1,
         is_pow2/1,
         timestamp/0]).

nextPow2(X) when is_integer(X) ->
  A = round(math:pow(2, round(math:log(X)/math:log(2)))),
  case A < X of
    true ->
      A * 2;
    false ->
      A
  end.

log2(X) when is_integer(X) ->
  round( math:log(X) / math:log(2) ).


is_pow2(X) when is_integer(X) ->
    ( (X > 0) and  ((X band (X - 1)) == 0) ).

%% @spec timestamp(Now) -> string()
%% @doc Generates a fancy looking timestamp, found on:
%%    http://erlang.2086793.n4.nabble.com/formatting-timestamps-td3594191.html
timestamp() -> 
  Now = now(),
    {_, _, Micros} = Now, 
    {{YY, MM, DD}, {Hour, Min, Sec}} = calendar:now_to_local_time(Now), 
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~p", 
                  [YY, MM, DD, Hour, Min, Sec, Micros]).


set_list_index( List, Index, Value ) ->
  {HeadList, [_|TailList]} = lists:split( Index - 1, List ),
  lists:append( HeadList, [Value | TailList] ).