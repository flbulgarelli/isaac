-module(majority).

-compile(export_all).

simple() ->
  fun({PositiveVotesCount, ElectorsCount}) ->
    PositiveVotesCount >= (ElectorsCount div 2) + 1 end.

ratio(PositiveVotesRatio) ->
 fun({PositiveVotesCount, ElectorsCount}) ->
  PositiveVotesCount / ElectorsCount >= PositiveVotesRatio end.

%% Consider absenses
%% Multi options; Percentage + difference to next value e.g. 40 + 10
%% Multi options; Max between most voted