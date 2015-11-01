-module(ouc_dashboard_h).
-export([init/3,
         handle/2,
         terminate/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("cowboy/include/http.hrl").
-endif.

-include("oacd_ouc.hrl").

-record(state, {tabs = []}).

init({_, http}, Req, Opts) ->
	TabsOpt = proplists:get_value(tabs, Opts, []),
    {ok, Req, #state{tabs = TabsOpt}}.

handle(Req, State) ->
	case ouc_session:get_session(Req) of
		{ok, Session, Req2} ->
			CustomOpts = build_custom_tab_opts(Session#session.username, State#state.tabs),
			%%UserTabs = get_tabs(Session#session.security_level, CustomOpts, State#state.tabs),
			%% use supervisor as we want to have all the tabs available
			UserTabs = get_tabs(supervisor, CustomOpts, State#state.tabs),
			lager:info("Tab conf for user ~p: ~p", [Session#session.username, UserTabs]),
			{ok, Bin} = ouc_dashboard_dtl:render([
				{is_debug, ouc_handler_util:get_config(frontend_debug)},
				{static_root, ouc_handler_util:get_config(frontend_static_root_uri)},
				{logout_code, Session#session.logout_code},
				{tabs, UserTabs},
				{active_tab_index, 0}
			]),
			{ok, Req3} = cowboy_req:reply(200, [], Bin, Req2),
			{ok, Req3, State};
		_ ->
			{ok, Req2} = ouc_handler_util:redirect(login, Req),
			{ok, Req2, State}
	end.

terminate(_Reason, _Req, _State) ->
	ok.

% Internal functions

get_tabs(SLevel, TabOpts, DefOpts) ->
	SVal = get_security_sval(SLevel),
	get_tabs(SVal, TabOpts, DefOpts, []).

get_tabs(_, [], _, Acc) ->
	lists:reverse(Acc);
get_tabs(SVal, [{Tab, Opts}|Rest], DefOpts, Acc) ->
	DefTabOpt = proplists:get_value(Tab, DefOpts, []),
	SLevel = proplists:get_value(security_level, DefTabOpt, agent),
	TabSVal = get_security_sval(SLevel),

	Acc1 = case TabSVal > SVal of
		true ->
			Acc;
		_ ->
			TabConfMap = [layout],
			GadgetConfMap = [{position, [col, row]}, {width, [colspan]}],

			TabConf = map_conf(Opts, TabConfMap),
			Gadgets = proplists:get_value(gadgets, Opts, []),
			GadgetConf = [{Name, map_conf(Conf, GadgetConfMap)} || {Name, Conf} <- Gadgets],

			[{Tab, TabConf, GadgetConf}|Acc]
	end,
	get_tabs(SVal, Rest, DefOpts, Acc1).

get_security_sval(admin) -> 2;
get_security_sval(supervisor) -> 1;
get_security_sval(_) -> 0. %% agent level by default

map_conf(Conf, ConfMap) ->
	lists:foldl(fun({Key, ConfKeys}, FoldAcc) ->
			case proplists:get_value(Key, Conf) of
				undefined ->
					FoldAcc;
				Val when is_tuple(Val) ->
					lists:zipwith(fun(X, Y) -> {Y, X} end, tuple_to_list(Val), ConfKeys) ++ FoldAcc;
				Val ->
					[ConfKey|_] = ConfKeys,
					[{ConfKey, Val} | FoldAcc]
			end;
		(Key, FoldAcc) when is_atom(Key) ->
			case proplists:get_value(Key, Conf) of
				undefined ->
					FoldAcc;
				Val ->
					[{Key, Val} | FoldAcc]
			end;
		(_, FoldAcc) ->
			FoldAcc
	end, [], ConfMap).

build_custom_tab_opts(Username, DefaultOpts) ->
	Db = ouc_db:get_db(reach),
	case Db:findOne(<<"user_layout">>, [{<<"agent">>, Username}]) of
		{ok, Entry} when length(Entry) > 0 ->
			Tabs = ej:get({"tabs"}, Entry),
			CustomOpts = lists:foldl(fun({Tab, DefaultConf}, Acc) ->
				TabConf = case ej:get({Tab}, Tabs) of
					undefined ->
						{Tab, DefaultConf};
					DbConf ->
						{Tab, tab_db_to_dashboard_conf(DbConf)}
				end,
				[TabConf | Acc]
			end, [], DefaultOpts),
			lists:reverse(CustomOpts);
		_ ->
			DefaultOpts
	end.

tab_db_to_dashboard_conf(DbConf) ->
	{array, GadgetEntries} = proplists:get_value(<<"gadgets">>, DbConf),
	Layout = proplists:get_value(<<"layout">>, DbConf),
	CustomGadgets = gadget_entries_to_opts(GadgetEntries),
	[{layout, binary_to_atom(Layout, utf8)}, {gadgets, CustomGadgets}].

gadget_entries_to_opts(Gadgets) ->
	lists:map(fun(Gadget) ->
		Name = proplists:get_value(<<"name">>, Gadget),
		{array, Position} = proplists:get_value(<<"position">>, Gadget),
		Width = proplists:get_value(<<"width">>, Gadget),
		{Name, [{position, list_to_tuple(Position)}, {width, Width}]}
	end, Gadgets).

-ifdef(TEST).
%% @todo update tests
mock_cowboy_modules() ->
    %% To update to cowboy 1.0.1 api
	meck:new(cowboy_tcp_transport),
	meck:expect(cowboy_tcp_transport, send, fun(socket, _) -> ok end),
	meck:new(cowboy_clock),
	meck:expect(cowboy_clock, rfc1123, 0, <<>>).

get_dashboard_test_() ->
	{setup, fun() ->
		meck:new(ouc_dashboard_dtl),
		meck:new(ouc_session),
		meck:new(mongoapi),
		meck:expect(mongoapi, new, fun(N, Db) -> {mongoapi, N, Db} end),
		meck:expect(mongoapi, findOne, fun(_,[{<<"agent">>,"1000"}],_) ->
			{ok, [{<<"tabs">>,
					[{<<"main">>,
						[{<<"gadgets">>,
							{array, [[{<<"name">>,<<"main_gadget">>},
								{<<"position">>, {array,[5,6]}},
								{<<"width">>, 3}]]}},
						 {<<"layout">>, <<"wide">>}]}]}]};
			(_,_,_) ->
				{ok, []}
		end),
		mock_cowboy_modules()
	end, fun(_) ->
		meck:unload()
	end, [{"get agent tab", fun() ->
		meck:expect(ouc_dashboard_dtl, render, fun(_) -> {ok, <<"test">>} end),
		meck:expect(ouc_session, get_session, fun(Req) -> {ok, #session{username = "1000", logout_code = "abcdef", security_level = agent}, Req} end),
		Req = #http_req{pid = self(), socket = socket, transport = cowboy_tcp_transport},
		Opts = [{tabs, [
			{<<"main">>, [{layout, narrow}, {gadgets, [{<<"main_gadget">>, [{position, {1,2}}, {width,1}]}]}]},
			{<<"sup">>, [{layout, wide}, {gadgets, [{<<"sup_gadget">>, [{position, {3,4}}, {width,2}]}]}, {security_level, supervisor}]}]}],

		{ok, Req2, State} = init({tcp, http}, Req, Opts),
		?assertEqual({ok, Req#http_req{resp_state = done}, State}, handle(Req2, State)),
		?assert(meck:called(ouc_dashboard_dtl, render, [[{is_debug, false},
					{static_root, ""},
					{logout_code, "abcdef"},
					{tabs, [{<<"main">>, [{layout, wide}], [{<<"main_gadget">>, [{colspan,3}, {col,5}, {row,6}]}]}]},
					{active_tab_index, 0}]])),
		?assert(meck:called(cowboy_tcp_transport, send, [socket, <<"test">>]))
	end}, {"get supervisor tabs", fun() ->
		meck:expect(ouc_dashboard_dtl, render, fun(_) -> {ok, <<"test">>} end),
		meck:expect(ouc_session, get_session, fun(Req) -> {ok, #session{username = "1001", logout_code = "abcdef", security_level = supervisor}, Req} end),
		Req = #http_req{pid = self(), socket = socket, transport = cowboy_tcp_transport},
		Opts = [{tabs, [
			{<<"main">>, [{layout, narrow}, {gadgets, [{<<"main_gadget">>, [{position, {1,2}}, {width, 1}]}]}]},
			{<<"sup">>, [{layout, wide}, {gadgets, [{<<"sup_gadget">>, [{position, {3,4}}, {width, 2}]}]}, {security_level, supervisor}]}]}],

		{ok, Req2, State} = init({tcp, http}, Req, Opts),
		?assertEqual({ok, Req#http_req{resp_state = done}, State}, handle(Req2, State)),
		?assert(meck:called(ouc_dashboard_dtl, render, [[{is_debug, false},
					{static_root, ""},
					{logout_code, "abcdef"},
					{tabs, [{<<"main">>, [{layout, narrow}], [{<<"main_gadget">>, [{colspan,1}, {col,1}, {row,2}]}]},
						{<<"sup">>, [{layout, wide}], [{<<"sup_gadget">>, [{colspan,2}, {col,3}, {row,4}]}]}]},
					{active_tab_index, 0}]])),
		?assert(meck:called(cowboy_tcp_transport, send, [socket, <<"test">>]))
	end}]}.

-endif.
