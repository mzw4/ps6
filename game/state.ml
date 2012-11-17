type state = {
	mutable current_phase: phase;
	mutable current_red_steammon: steammon option;
	mutable current_blue_steammon: steammon option;
	mutable game_data: game_status_data option;
	}
	
type phase = 
	| Init
	| SelectSteammon
	| SelectItems
	| Battle

let create : state = 
	{current_phase = Init;
	current_red_steammon = None;
	current_blue_steammon = None;
	game_data = None;
	}

let set_phase (st: state) (ph: phase) : unit =
	st.phase <- ph;

let set_red_starter (st: state) (steam: steammon) : unit =
	st.current_red_steammon <- steam;
	
let set_blue_starter (st: state) (steam: steammon) : unit =
	st.current_blue_steammon <- steam;

let set_game_data (st: state) (data: game_status_data) : unit =
	st.game_data <- data;


	

	

	
	