-module(sort).
-export([startSort/2, fileSort/2]).

startSort(P, Xs) ->
    io:format("~p", [sort(P,Xs)]).
sort(0,Xs) ->
  lists:sort(Xs); % работает быстрее
  %qsort(Xs)
sort(_,[]) ->
  [];
sort(D,[X|Xs]) ->
  Parent = self(),
  Ref = make_ref(),
  Grtr = [Y || Y <- Xs, Y >= X],
  spawn_link(fun() -> Parent ! {Ref,sort(D-1, Grtr)} end),
  sort(D-1,[Y || Y <- Xs, Y < X]) ++ [X] ++
    receive {Ref,Greater} -> Greater end.

qsort([]) ->
  [];
qsort([X|Xs]) ->
  qsort([Y || Y <- Xs, Y<X]) ++ [X] ++ qsort([Y || Y <- Xs, Y>=X]).

fileSort( P, Filename ) ->
  case file:open( Filename, [read] ) of
    {ok, IoDevice} ->
      StartTime = erlang:system_time(millisecond),
      List = readFile2( IoDevice, [] ),
      ReadTime = erlang:system_time(millisecond),
      io:format("Read time: ~p~n", [(ReadTime - StartTime)/1000]),
      Sorted = sort(P, List),
      SortTime = erlang:system_time(millisecond),
      io:format("Sort time: ~p~n", [(SortTime - ReadTime)/1000]),
      writeFile4( "solution.txt", Sorted),
      EndTime = erlang:system_time(millisecond),
      io:format("Write time: ~p~n", [(EndTime - SortTime)/1000]),
      io:format("Total time: ~p~n", [(EndTime - StartTime)/1000]),
      file:close(IoDevice);
    {error, Reason} ->  io:format("~p", [Reason]) % http://erlang.org/doc/man/inet.html#posix-error-codes
  end.

% Крайне медленно
readFile( IoDevice, List ) ->
  case io:fread( IoDevice, [], "~f") of
    eof -> List;
    {ok, [Double]} -> readFile( IoDevice, [Double | List] );
    {error, Reason} -> io:format( "io:fread error: ~w~n", [Reason] ),
      List
  end.

% Сойдёт
readFile2( IoDevice, List ) ->
  case file:read_line(IoDevice) of
    {ok, Data} ->
      Res = lists:map(
        fun(String) ->
          list_to_float(String) end,
        string:tokens(Data, " \t\n")),
      readFile2(IoDevice, lists:append(List, Res));
    eof -> List
  end.

% Медленно
writeFile( Name, List ) ->
  {ok, IoDevice} = file:open(Name, [write]),
  lists:foreach(
    fun(Line) -> io:fwrite(IoDevice, "~p ", [Line])
    end,
    List),
  file:close(IoDevice).

% Всё ещё недостаточно быстро
writeFile3( Name, List ) ->
  file:write_file(Name, lists:map(fun(Term) -> io_lib:format("~p ", [Term]) end, List)).

% Сойдёт
writeFile4( Name, List ) ->
  file:write_file(Name, lists:map(fun(Term) -> float_to_list(Term)++" " end, List)).