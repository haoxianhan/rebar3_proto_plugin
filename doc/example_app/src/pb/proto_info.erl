-module(proto_info).

-export([get_msg_handle/1]).

get_msg_handle(4) -> handle_client;
get_msg_handle(1) -> handle_shop;
get_msg_handle(5) -> handle_client;
get_msg_handle(2) -> handle_shop;
get_msg_handle(3) -> handle_shop;
get_msg_handle(_) -> undefined.