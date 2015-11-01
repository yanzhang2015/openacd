%% ===========================================================================
%% @copyright 2013-2014 eZuce Inc.
%% @author Jan Vincent Liwanag, Lou Uysison, Danna Aduna
%% @doc Rolling stats backend - Writes data in MongoDB 
%% @end
%% ===========================================================================
-module(ouc_rstat_store_mongo).
-behavior(ouc_rstat_store).

-include("ouc_rstat.hrl").

-define(TAB, ouc_rstat).
-define(VSN, 2).
%% ===========================================================================
%% Public API
%% ===========================================================================

-export([
	get_store/0,
	init/1,
	increment/3,
	set_if_max/3,
	read/2,
	read_range/2,
	needs_cache/0
]).


%% ---------------------------------------------------------------------------
-spec get_store() -> tuple().
%% @doc Returns the current store ?? 
%% @end
%% ---------------------------------------------------------------------------
get_store() ->
	{?MODULE, void}.

%% ---------------------------------------------------------------------------
-spec init(Arg::any()) -> {ok, void}.
%% @doc Initialise the connection to MongoDB and ensures the required indexes
%%      exist in MongoDB.
%%      Returns {ok, void}
%% @end
%% --------------------------------------------------------------------------- 
init(_) ->
	Db = ouc_db:get_db(openacd),
	try Db:ensureIndex(<<"rstats">>, get_indexes()) of
		Res -> Res
	catch
		_Error:Reason -> lager:error("Error creating index with reason ~p", [Reason])
	end,
	{ok, void}.


%% ---------------------------------------------------------------------------
-spec increment(_Old, K, N) -> void 
    when 
        K :: {T, {From, To}, Nxds},
        T :: tuple(),
        From :: instant(),
        To :: instant(),
        Nxds :: [{atom(), list()}],
        N :: integer().
%% @doc Increment the value of the key K with the value N. <br />
%%      From and To are represent timestamps in unixts, defined in ouc_time. <br />
%%      T tuple of type stat <br />
%%      N designates the increment ammount. <br />
%% @end
%% ---------------------------------------------------------------------------
increment(_, K, N) ->
	KJ = k_to_json(K),

	% ouc_db:exec_cmd(reach, [{<<"findAndModify">>, <<"rstats">>},
	% 	{<<"query">>, KJ},
	% 	{<<"update">>, [{<<"$inc">>, [{<<"val">>, N}]}]},
	% 	{<<"upsert">>, true}]),
	Db = ouc_db:get_db(openacd),
	Db:update(<<"rstats">>, KJ, [{<<"$inc">>, [{<<"val">>, N}]}], [upsert]),
	void.

%% ---------------------------------------------------------------------------
-spec set_if_max(_OldState::any(), K, N) -> void
    when 
        K :: {T, {From, To}, Nxds},
        T :: tuple(),
        From :: instant(),
        To :: instant(),
        Nxds :: [{atom(), list()}],
        N :: integer().
%% @doc The same as with increment, but now instead of incrementing with N, 
%%      N is the new value of the val member of the mongo document. 
%% @end
%% ---------------------------------------------------------------------------
set_if_max(_, K, N) ->
	KJ = k_to_json(K),

	% ouc_db:exec_cmd(reach, [{<<"findAndModify">>, <<"rstats">>},
	% 	{<<"query">>, KJ},
	% 	{<<"update">>, [{<<"$push">>, [{<<"vals">>, N}]}]},
	% 	{<<"upsert">>, true}]),
	Db = ouc_db:get_db(openacd),
	Db:update(<<"rstats">>, KJ, [{<<"$push">>, [{<<"vals">>, N}]}], [upsert]),
	void.

%% ---------------------------------------------------------------------------
-spec read(_OldState::any(), K) -> {atom(), V::integer()}
    when 
        K :: {T, {From, To}, Nxds},
        T :: tuple(),
        From :: instant(),
        To :: instant(),
        Nxds :: [{atom(), list()}].
%% @doc Returns a value in the db, given the Mongo query K 
%% @end
%% ---------------------------------------------------------------------------
read(_, K) ->
	KJ = k_to_json(K),
	Db = ouc_db:get_db(openacd),

	V = try
			case Db:findOne(<<"rstats">>, KJ) of
				{ok, [_|_] = P} ->
					case get_type(K) of
						max ->
							resolve_max(KJ, P);
						_ ->
							trunc(proplists:get_value(<<"val">>, P, 0))
					end;
				_ ->
					0
			end
		catch
			Error:Reason -> lager:error("Error ~p with reason ~p occured while trying to read from mongo",[Error, Reason]),
					0
		end,
	{void, V}.

%% ---------------------------------------------------------------------------
-spec read_range(any(), K) -> {void, V::integer()}
    when 
        K :: {T, {From, To}, Nxds},
        T :: tuple(),
        From :: instant(),
        To :: instant(),
        Nxds :: [{atom(), list()}].
%% @doc read_range currently does not take into account multiple
%%      rules with same coverage but different pivot
%% @todo unsure of the returned type
%% @end
%% ---------------------------------------------------------------------------
read_range(_, K) ->
	GroupVal = case get_type2(K) of
		max ->
			{<<"$push">>, <<"$vals">>};
		total ->
			{<<"$sum">>, <<"$val">>};
		_ ->
			lager:info("K : ~p", [K]),
			{undefined, undefined}
	end,

	Match = k_range_to_match(K),

	Res = ouc_db:exec_cmd(reach, [
		{<<"aggregate">>, <<"rstats">>},
		{<<"pipeline">>, {array, [
			[{<<"$match">>, Match}],
			[{<<"$group">>, [{<<"_id">>, <<>>}, {<<"vals">>,
				[GroupVal]
			}]}]
		]}}
	]),

	V = try
			case lists:keyfind(<<"result">>, 1, Res) of
				{_, {array, [Entry]}} ->
					{_, AggRes} = lists:keyfind(<<"vals">>, 1, Entry),
					%%lager:debug("Aggregate result: ~p", [AggRes]),
					case get_type2(K) of
						max ->
							get_max(AggRes);
						_ ->
							AggRes
					end;
				_ ->
					0
			end
		catch
			Error:Reason -> lager:error("Error ~p with reason ~p occured while trying to read_range from mongo",[Error, Reason]),
					0
		end,
	{void, V}.

%% ---------------------------------------------------------------------------
%% @doc Just returns true
%% @end
%% ---------------------------------------------------------------------------
needs_cache() -> true.

%% ===========================================================================
%% Internal functions
%% ===========================================================================

k_to_json({T, {From, To}, Ndxs}) ->
	StringList = [atom_to_list(A) || A <- tuple_to_list(T)],
	TString = string:join(StringList, ":"),
	Cov = To - From + 1,

    NdxsO = [ parse_ndx(NdxsKey, NdxsVal) || {NdxsKey, NdxsVal} <- Ndxs ],

	[
		{<<"type">>, TString},
		{<<"from">>, ouc_time:unixts_to_erlts(From)},
		{<<"cov">>, Cov},
		{<<"ndxs">>, NdxsO},
		{<<"vsn">>, ?VSN}
	].

parse_ndx(NdxsKey, NdxsVal) ->
    ToBinaryFun = fun(X) ->
        if
            is_list(X) -> erlang:list_to_binary(X);
            is_atom(X) -> erlang:atom_to_binary(X, utf8);
            is_integer(X) -> erlang:list_to_binary(integer_to_list(X));
            true -> erlang:atom_to_binary(X, utf8)
        end
    end,
    Result = try
        {ToBinaryFun(NdxsKey), ToBinaryFun(NdxsVal)} of
            Res -> Res
        catch
            Error:Reason -> lager:error("Failed to parse the Ndxs with key: ~p and val ~p due to ~p and ~p",[NdxsKey, NdxsVal, Error, Reason])
    end,
    Result.

k_range_to_match({T, {From, To}, Cov, Ndxs}) ->
	StringList = [atom_to_list(A) || A <- tuple_to_list(T)],
	TString = string:join(StringList, ":"),

	NdxsO = [{atom_to_binary(X, utf8), list_to_binary(Y)}
		|| {X, Y} <- Ndxs],

	[
		{<<"type">>, TString},
		{<<"from">>, [{<<"$gte">>, ouc_time:unixts_to_erlts(From)},
					  		{<<"$lt">>, ouc_time:unixts_to_erlts(To - Cov + 1)}]},
		{<<"cov">>, Cov},
		{<<"ndxs">>, NdxsO},
		{<<"vsn">>, ?VSN}
	].

get_indexes() ->
	[
		{<<"cov">>,1},
		{<<"from">>,1},
		{<<"ndxs">>,1},
		{<<"type">>,1}
	].

get_type({{max, _, _}, _, _}) -> max;
get_type({{total, _, _}, _, _}) -> total;
get_type(_) -> other.

get_type2({{max, _, _}, _, _, _}) -> max;
get_type2({{total, _, _}, _, _, _}) -> total;
get_type2(_) -> other.

get_max({array, PropList}) ->
	NestedVals = proplists:get_all_values(array, PropList),
	Vals = lists:flatten(NestedVals),
	lists:max(Vals).

resolve_max(KJ, P) ->
	Ns = case proplists:get_value(<<"vals">>, P) of
		{array, Z} -> Z;
		_ -> []
	end,

	case Ns of
		[] ->
			0;
		[E] ->
			trunc(E);
		L ->
			Max = lists:max(L),
			catch store_max(KJ, Max),
			trunc(Max)
	end.

store_max(KJ, Max) ->
	ouc_db:exec_cmd(reach, [{<<"findAndModify">>, <<"rstats">>},
		{<<"query">>, KJ},
		{<<"update">>, [{<<"$set">>, [{<<"vals">>, {array, [Max]}}]}]},
		{<<"upsert">>, true}]).
