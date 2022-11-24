-module(proto_info).

-export([get_msg_name/1,
         get_msg_code/1,
         get_msg_pbmodule/1]).

get_msg_name(1) -> req_heartbeat;
get_msg_name(3) -> req_shop_info;
get_msg_name(2) -> resp_heartbeat;
get_msg_name(4) -> resp_shop_info;
get_msg_name(5) -> shop_info;
get_msg_name(_) -> undefined.

get_msg_code(req_heartbeat) -> 1;
get_msg_code(req_shop_info) -> 3;
get_msg_code(resp_heartbeat) -> 2;
get_msg_code(resp_shop_info) -> 4;
get_msg_code(shop_info) -> 5;
get_msg_code(_) -> undefined.

get_msg_pbmodule(1) -> pb_client;
get_msg_pbmodule(3) -> pb_shop;
get_msg_pbmodule(2) -> pb_client;
get_msg_pbmodule(4) -> pb_shop;
get_msg_pbmodule(5) -> pb_shop;
get_msg_pbmodule(_) -> undefined.