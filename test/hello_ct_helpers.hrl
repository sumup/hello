-define(equal(Expected, Actual),
    (fun (Expected@@@, Expected@@@) -> true;
         (Expected@@@, Actual@@@) ->
             ct:fail("MISMATCH(~s:~b, ~s)~nExpected: ~p~nActual:   ~p~n",
             [?FILE, ?LINE, ??Actual, Expected@@@, Actual@@@])
     end)(Expected, Actual)).

-define(match(Guard, Expr),
        ((fun () ->
                  case (Expr) of
                      Guard -> ok;
                      V -> ct:fail("MISMATCH(~s:~b, ~s)~nExpected: ~p~nActual:   ~p~n",
                                   [?FILE, ?LINE, ??Expr, ??Guard, V])
                  end
          end)())).

-define(proper_qc(Prop, Options),
        true = proper:quickcheck(Prop, [{on_output, fun (Format, Data) ->
                                                        io:format(user, Format, Data)
                                                    end}
                                        | Options])).
