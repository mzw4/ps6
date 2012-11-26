open Definitions

type state

val create: state 

val set_game_data: state -> game_status_data -> unit

val already_selected: state -> steammon -> bool

val team_full: state -> color -> bool

val inventory_contains: state -> color -> item -> bool

val is_active: state -> color -> steammon -> bool

val active_fainted: state -> color -> bool

val game_result: state -> game_result option

val add_steammon: state -> color -> steammon -> unit

val switch_steammon: state -> color -> steammon -> unit

val set_inventory: state -> color -> int list -> unit

val add_item: state -> color -> item -> unit

val remove_item: state -> color -> item -> unit

val attack: state -> color -> attack -> unit

val use_item: state -> color -> item -> steammon -> unit
