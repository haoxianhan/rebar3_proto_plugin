-module(proto_info_custom).

-export([
          fun_list/0
        ]).

fun_list() ->
    [fun get_msg_handle/1].


get_msg_handle(MetaList) ->
    Fun = fun(PbModule) ->
                  case atom_to_list(PbModule) of
                      "pb_" ++ Proto ->
                          list_to_atom("handle_"++Proto);
                      _ ->
                          undefined
                  end
          end,
    ClausesMapsList = [ #{args => [MsgCode], return => Fun(PbModule)}
                        || #{msg_code := MsgCode, pb_module := PbModule} <- MetaList],
    #{fun_name => ?FUNCTION_NAME,
      clauses => ClausesMapsList}.

