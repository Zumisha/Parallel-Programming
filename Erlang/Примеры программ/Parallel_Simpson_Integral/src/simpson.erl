% http://www1.erlang.org/doc/reference_manual/errors.html#exit_reasons

-module(simpson).
-author("User").

-export( [ test3/4, seqSum/6 ] ).

test3(A, B, Eps, P) ->
  StartTime = erlang:system_time(millisecond),
  io:format("~p\n", [simpson(fun fun3/1, A, B, Eps, P)]),
  EndTime = erlang:system_time(millisecond),
  io:format("~p", [(EndTime- StartTime)/1000]).

fun1(X) ->
  (X * X + 1.0) * math:cos(0.5 * X).

fun2(X) ->
  (X * X + X * 3.0 + 11.0) / (X * X + 11.0 * X - 7.0).

fun3(X) ->
  math:sin(0.5 * X * X * X) * math:sin(0.25 * X * X) * math:sin(0.125 * X).

simpson(F , A, B, Eps, P) ->
  M = (A + B) / 2,
  H = (B - A) / 2,
  Af = F(A),
  Bf = F(B),
  OddSum = F(M),
  Integral = H * (Af + Bf + 4 * OddSum) / 3,
  simpsonCalc(F, A, B, Eps, P, H/2, Af, Bf, OddSum, Integral).

simpsonCalc(F, A, B, Eps, P, H, Af, Bf, EvenSum, LastIntegral) ->
  % io:format("~p; ~p; ~p; ~p; ~p; ~p; ~p; ~p; ~p; ~p\n", [F, A, B, Eps, P, H, Af, Bf, EvenSum, LastIntegral]),
  SumH = 2.0 * H,
  OddSum = parSumSpawn(F, P, 0, A + H, B - H, SumH, SumH * P),
  Integral = H * (Af + Bf + 2 * EvenSum + 4 * OddSum) / 3,
  case runge(LastIntegral, Integral, Eps) of
    false -> simpsonCalc(F, A, B, Eps, P, H / 2.0, Af, Bf, (EvenSum + OddSum), Integral);
    true -> Integral
  end.

runge(LastIntegral, Integral, Eps) ->
  abs(LastIntegral - Integral)/15 < Eps.

seqSum(Pid, F, A, B, H, Sum) ->
  if
    A > B ->
      Pid ! {sumResult, Sum};
    true -> seqSum(Pid, F, A + H, B, H, Sum + F(A))
  end.

parSumSpawn(F, P, SpawnCounter, A, B, H, Ph) ->
  if
    (P > SpawnCounter) and (A =< B) ->
      spawn(simpson, seqSum, [self(), F, A, B, Ph, 0.0]),
      parSumSpawn(F, P, SpawnCounter + 1, A + H, B, H, Ph);
    true -> parSumReceive(SpawnCounter, 1, 0.0)
  end.

parSumReceive(P, ReceiveCounter, Sum) ->
  receive
    {sumResult, Result} ->
      if
        P > ReceiveCounter ->
          parSumReceive(P, ReceiveCounter + 1, Sum + Result);
        true -> Sum + Result
      end
  end.