%%%-------------------------------------------------------------------
%%% @author amir
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Aug 2020 11:13 AM
%%%-------------------------------------------------------------------
-module(funcGenerator).
-author("amir").
%% API
-export([generatePolynom/2,solveP/3,solveS/3,generateSin/2]).

generatePolynom(0,Polynom) -> lists:reverse([{(rand:uniform_real() - 0.5) ,0} | Polynom]);
generatePolynom(Length, Polynom) -> generatePolynom(Length - 1, [{(rand:uniform_real() - 0.5) / math:pow(100,Length),Length} | Polynom]).

solveP([],X,Acc) -> {X,Acc};
solveP(Func,X,Acc) -> solveP(tl(Func),X,Acc + partialSolveP(hd(Func),X)).

partialSolveP({Coeff,Power},X) -> Coeff*math:pow(X,Power).

generateSin(0,Function) -> lists:reverse([{(rand:uniform(2000) - 1000) / 2000,0} | Function]);
generateSin(NumOfElements,Function) -> generateSin(NumOfElements -1,[{(rand:uniform(2000) - 1000) , generatePolynom(NumOfElements,[])}|Function]).


solveS([{_ElemA,ElemB}|[]],X,Acc) -> {X,ElemB + Acc};
solveS(Func,X,Acc) ->
  {ElemA,ElemB} = hd(Func),
  {_ElemC,ElemD} = solveP(ElemB,X,0),
  solveS(tl(Func),X,Acc + ElemA*math:sin(ElemD)).


