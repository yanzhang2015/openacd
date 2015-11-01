-module(ouc_error).
-export([err/1]).

err({code, Code}) when is_binary(Code) ->
	err_code(list_to_integer(binary_to_list(Code)));
err({code, Code}) when is_integer(Code) ->
	err_code(Code).

err_code(4000) ->
	{4000, "Duplicate login"};
err_code(Code) ->
	{Code, []}.
