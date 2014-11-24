-module(majority).

-compile(export_all).

simple({PositiveVotesCount, ElectorsCount}) ->
  PositiveVotesCount > ElectorsCount / 2 + 1.

ratio(PositiveVotesRatio) ->
 fun({PositiveVotesCount, ElectorsCount}) ->
  PositiveVotesCount / ElectorsCount > PositiveVotesRatio end.

%% Consider absenses
%% Multi options; Percentage + difference to next value e.g. 40 + 10
%% Multi options; Max between most voted