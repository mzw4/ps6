type state = {
	current_phase: phase;
	current_red_steammon: steammon;
	current_blue_steammon: steammon;
	game_data: game_status_data;
	
	}
	
type phase = 
	| Init
	| SelectSteammon
	| SelectItems
	| Battle
