-module(ouc_rpc_util).

-include("ouc_rstat.hrl").

-export([
	my_rstats_to_json/3
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

my_rstats_to_json(Snap, Login, Clients) ->
	RNames = ouc_rstat_snapshot:get(Snap, rnames),
	AllEntry = all_entry(RNames, Snap, Login, Clients),
	ClientsEntry = clients_entry(Clients, RNames, Snap, Login),
	{[{all, AllEntry}, {clients, ClientsEntry}]}.

%% internal

all_entry(RNames, Snap, Login, Clients) ->
	MyClientsJNdx = {join, [[{agent, Login}, {client, Cl}] || Cl <- Clients]},
	ClientsJNdx = {join, [[{client, Cl}] || Cl <- Clients]},
	[rbp_to_entry(RName, Snap, ClientsJNdx, MyClientsJNdx)
		|| RName <- RNames].

clients_entry(Clients, RNames, Snap, Login) ->
	[client_entry(Client, RNames, Snap, Login) || Client <- Clients].

client_entry(Client, RNames, Snap, Login) ->
	ClientNdx = [{client, Client}],
	MyClientNdx = [{agent, Login}, {client, Client}],

	{[{client, list_to_binary(Client)},
	{rstats, [rbp_to_entry(RName, Snap, ClientNdx, MyClientNdx)
		|| RName <- RNames]}]}.

rbp_to_entry(RName, Snap, ClNdx, MyClNdx) ->
	GetVal = fun(P, Ndx) ->
		ouc_rstat_snapshot:get_stat_val(Snap, RName, P, Ndx)
	end,

	Occ = GetVal(?PROP_PcS_OCCUPANCY, ClNdx),
	MCD = GetVal(?PROP_ME_CALL_DURATION, ClNdx),
	CPT = GetVal(?PROP_AvE_CALL_DURATION, ClNdx),
	AWA = GetVal(?PROP_AvE_WAIT_DURATION, ClNdx),

	MyACD = GetVal(?PROP_AvE_CALL_DURATION, MyClNdx),
	MyOcc = GetVal(?PROP_PcS_OCCUPANCY, MyClNdx),

	Es = {[
		{occupancy, Occ},
		{cpt, CPT},
		{longest_call_duration, MCD},
		{average_wait_duration, AWA},
		{my_cpt, MyACD},
		{my_occupancy, MyOcc}
	]},

	{[
		{duration, RName},
		{data, Es}
	]}.

%% tests

-ifdef(TEST).

% my_rstats_to_json_test() ->
% 	?assertEqual(
% 		{[
% 			{all, [{[
% 				{duration, last_30m},
% 				{from, 100},
% 				{to, 200},
% 				{data, {[
% 					{occupancy, 90.0},
% 					{cpt, 5.0},
% 					{longest_call_duration, 25},
% 					{average_wait_duration, 10},
% 					{my_cpt, 4.5},
% 					{my_occupancy, 88.0}
% 				]}}
% 			]}]},
% 			{clients, []}
% 		]},
% 		my_rstats_to_json({
% 			[{last_30m, {100, 200},
% 				{[
% 					{?PROP_PcS_OCCUPANCY, 90.0},
% 					{?PROP_ME_CALL_DURATION, 25},
% 					{?PROP_AvE_CALL_DURATION, 5.0},
% 					{?PROP_AvE_WAIT_DURATION, 10}
% 				],
% 				[
% 					{?PROP_AvE_CALL_DURATION, 4.5},
% 					{?PROP_PcS_OCCUPANCY, 88.0}
% 				]}

% 			}],

% 			%% Client rstats
% 			[]
% 			})
% 	).

-endif.
