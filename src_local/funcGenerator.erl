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
-define(sinFreq,100).
%% API
-export([generatePolynom/2,solveP/3,solveS/2,generateSin/2]).

generatePolynom(0,Polynom) -> lists:reverse([{rand:uniform(100) ,0} | Polynom]);
generatePolynom(Length, Polynom) ->
  %X = erlang:floor(math:pow(10,Length)),
  %generatePolynom(Length - 1, [{(rand:uniform_real() - 0.5) / rand:uniform(X) ,Length} | Polynom]).
  generatePolynom(Length -1 , [{rand:uniform(10) -  10 ,Length} | Polynom]).

solveP([],X,Acc) -> {X,Acc};
solveP(Func,X,Acc) -> solveP(tl(Func),X,Acc + partialSolveP(hd(Func),X)).

partialSolveP({Coeff,Power},X) -> Coeff*math:pow(X,Power).

%generateSin(0,Function) -> lists:reverse([{(rand:uniform(2000) - 1000) / 2000,0} | Function]);
generateSin(Coef,Function) ->{Coef,rand:uniform(250)}.
% generateSin(NumOfElements,Function) -> generateSin(NumOfElements -1,[{(rand:uniform(1000) - 500) , generatePolynom(NumOfElements,[])}|Function]).


%solveS([{_ElemA,ElemB}|[]],X,Acc) -> {X,ElemB + Acc};
solveS({Coeff,N},X) -> {X,erlang:floor(Coeff*math:sin(X/(?sinFreq*math:pi())) + N)}.


