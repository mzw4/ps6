type state = {
	mutable current_phase: phase;
	mutable game_data: game_status_data option;
	}
	
type phase = 
	| Init
	| SelectSteammon
	| SelectItems
	| Battle

let create : state = 
	{current_phase = Init;
	game_data = None;
	}

let set_phase (st: state) (ph: phase) : unit =
	st.phase <- ph;

let set_game_data (st: state) (data: game_status_data) : unit =
	st.game_data <- data;


	

	

	
	