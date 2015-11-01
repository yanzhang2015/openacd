-module(ouc_cowboy_util).
-export([range/1]).

%% @doc Parse range header according rfc 2616.
-spec range(binary()) -> {Unit, [Range]} | {error, badarg} when
		Unit :: binary(),
		Range :: {non_neg_integer(), non_neg_integer() | infinity} | neg_integer().
range(Data) ->
	token_ci(Data, fun range/2).

range(Data, Token) ->
	whitespace(Data,
		fun(<<"=", Rest/binary>>) ->
			case list(Rest, fun range_beginning/2) of
				{error, badarg} ->
					{error, badarg};
				Ranges ->
					{Token, Ranges}
			end;
		   (_) ->
			{error, badarg}
		end).

range_beginning(Data, Fun) ->
	range_digits(Data, suffix,
		fun(D, RangeBeginning) ->
			range_ending(D, Fun, RangeBeginning)
		end).

range_ending(Data, Fun, RangeBeginning) ->
	whitespace(Data,
		fun(<<"-", R/binary>>) ->
			case RangeBeginning of
				suffix ->
					range_digits(R, fun(D, RangeEnding) -> Fun(D, -RangeEnding) end);
				_ ->
					range_digits(R, infinity,
						fun(D, RangeEnding) ->
							Fun(D, {RangeBeginning, RangeEnding})
						end)
			end;
		   (_) ->
			{error, badarg}
		end).

-spec range_digits(binary(), fun()) -> any().
range_digits(Data, Fun) ->
	whitespace(Data,
		fun(D) ->
			digits(D, Fun)
		end).

-spec range_digits(binary(), any(), fun()) -> any().
range_digits(Data, Default, Fun) ->
	whitespace(Data,
		fun(<< C, Rest/binary >>) when C >= $0, C =< $9 ->
			digits(Rest, Fun, C - $0);
		   (_) ->
			Fun(Data, Default)
		end).


%% @doc Parse a list of the given type.
-spec list(binary(), fun()) -> list() | {error, badarg}.
list(Data, Fun) ->
	case list(Data, Fun, []) of
		{error, badarg} -> {error, badarg};
		L -> lists:reverse(L)
	end.

%% From the RFC:
%% <blockquote>Wherever this construct is used, null elements are allowed,
%% but do not contribute to the count of elements present.
%% That is, "(element), , (element) " is permitted, but counts
%% as only two elements. Therefore, where at least one element is required,
%% at least one non-null element MUST be present.</blockquote>
list(Data, Fun, Acc) ->
	whitespace(Data,
		fun (<<>>) -> Acc;
			(<< $,, Rest/binary >>) -> list(Rest, Fun, Acc);
			(Rest) -> Fun(Rest,
				fun (D, I) -> whitespace(D,
						fun (<<>>) -> [I|Acc];
							(<< $,, R/binary >>) -> list(R, Fun, [I|Acc]);
							(_Any) -> {error, badarg}
						end)
				end)
		end).

%% @doc Skip whitespace.
-spec whitespace(binary(), fun()) -> any().
whitespace(<< C, Rest/binary >>, Fun)
		when C =:= $\s; C =:= $\t ->
	whitespace(Rest, Fun);
whitespace(Data, Fun) ->
	Fun(Data).

%% @doc Parse a case-insensitive token.
%%
%% Changes all characters to lowercase.
-spec token_ci(binary(), fun()) -> any().
token_ci(Data, Fun) ->
	token(Data, Fun, ci, <<>>).

%% @doc Parse a token.
-spec token(binary(), fun(), ci | cs, binary()) -> any().
token(<<>>, Fun, _Case, Acc) ->
	Fun(<<>>, Acc);
token(Data = << C, _Rest/binary >>, Fun, _Case, Acc)
		when C =:= $(; C =:= $); C =:= $<; C =:= $>; C =:= $@;
			 C =:= $,; C =:= $;; C =:= $:; C =:= $\\; C =:= $";
			 C =:= $/; C =:= $[; C =:= $]; C =:= $?; C =:= $=;
			 C =:= ${; C =:= $}; C =:= $\s; C =:= $\t;
			 C < 32; C =:= 127 ->
	Fun(Data, Acc);
token(<< C, Rest/binary >>, Fun, Case = ci, Acc) ->
	C2 = cowboy_bstr:char_to_lower(C),
	token(Rest, Fun, Case, << Acc/binary, C2 >>);
token(<< C, Rest/binary >>, Fun, Case, Acc) ->
	token(Rest, Fun, Case, << Acc/binary, C >>).

-spec digits(binary(), fun()) -> any().
digits(<< C, Rest/binary >>, Fun)
		when C >= $0, C =< $9 ->
	digits(Rest, Fun, C - $0);
digits(_Data, _Fun) ->
	{error, badarg}.

-spec digits(binary(), fun(), non_neg_integer()) -> any().
digits(<< C, Rest/binary >>, Fun, Acc)
		when C >= $0, C =< $9 ->
	digits(Rest, Fun, Acc * 10 + (C - $0));
digits(Data, Fun, Acc) ->
	Fun(Data, Acc).
