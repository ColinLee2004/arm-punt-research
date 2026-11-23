# Data Dictionary

The main file, `data/plays_2022_onward.csv.gz`, contains selected raw nflverse PBP fields, joined FTN charting fields, game metadata, and derived drive/return fields. The focused third-down and punt CSVs use the same schema.

## Keys and game/play state

- `game_id`: nflverse game identifier and main join key. `espn_game_id`: ESPN ID crosswalk.
- `nflverse_play_id` / `play_id`: nflverse play number; use `nflverse_play_id` to join FTN. `fixed_drive`: nflverse drive number.
- `season`, `season_type`, `week`, `game_date`, `qtr`: season, game type, week, date, and quarter.
- `quarter_seconds_remaining`, `half_seconds_remaining`, `game_seconds_remaining`, `time`: play clock context.
- `down`, `ydstogo`, `yardline_100`: down, yards to go, and distance from the offense's opponent goal at play start.
- `posteam`, `defteam`, `home_team`, `away_team`: team abbreviations for joins/auditing, not primary-model team effects.

## Pass, penalty, and turnover fields

- `pass_attempt`, `complete_pass`, `incomplete_pass`, `sack`, `qb_scramble`: pass/dropback indicators.
- `air_yards`, `yards_after_catch`, `yards_gained`: target depth, recorded YAC, and total play yards. Use total gain for the resulting ball spot.
- `pass_length`, `pass_location`: source pass-depth and target-location labels.
- `interception`, `interception_return_yards`, `return_yards`, `return_team`, `return_touchdown`: interception/return fields. `interception_return_yards` is derived from generic `return_yards` on interception plays.
- `penalty`, `penalty_type`, `penalty_team`, `penalty_yards`, `first_down_penalty`: penalty indicators, type/team, yards, and penalty first-down flag. Interpret together with the description and resulting play state.
- `desc`: source description for auditing unusual outcomes and penalties.

## Punt and value fields

- `punt_attempt`: punt indicator used for the focused punt file.
- `punt_blocked`, `punt_inside_twenty`, `punt_in_endzone`, `punt_out_of_bounds`, `punt_downed`, `punt_fair_catch`, `touchback`: punt/kick outcome flags.
- `punt_return_yards`: derived from generic return yards on punt attempts; retain zero and negative values as recorded.
- `ep`, `epa`, `wp`, `home_wp`, `wpa`: nflverse source expected-points and win-probability outputs for actual game context. Revalue for the neutral baseline.
- `home_score`, `away_score`, `posteam_score`, `defteam_score`, `score_differential`: source score context. Timeout fields include `posteam_timeouts_remaining`, `defteam_timeouts_remaining`, `home_timeouts_remaining`, and `away_timeouts_remaining`.
- `special_teams_play`, `series_success`, `series_result`, `fixed_drive_result`, `drive_start_yard_line`, `drive_end_yard_line`, `drive_end_transition`: additional play/series/drive fields.

## Drive starts

`current_drive_start_yardline` is the current drive's starting distance from that offense's opponent goal. `next_drive_start_yardline` is the actual next recorded drive's start in the same coordinate system; `next_drive_own_yardline` converts it to yards from that offense's own goal (`100 - next_drive_start_yardline`). `next_drive_team`, `next_drive_result`, and `next_drive_is_score` describe that drive.

For a punt or immediate turnover, the next drive usually represents the receiving side's location after returns and enforcement. Check the next-drive team and play result because a return touchdown, blocked punt, or unusual sequence can change what the next drive represents. For a completion that keeps possession, the next-drive fields describe a later realized result, not the immediate pass result or a predictor. `drives_2022_onward.csv` has one row per nflverse drive, with start/end locations in the same coordinate system.

## FTN fields

FTN fields use the `ftn_` prefix. Main study fields are `ftn_is_contested_ball`, `ftn_is_catchable_ball`, `ftn_is_throw_away`, `ftn_is_drop`, `ftn_is_created_reception`, `ftn_is_interception_worthy`, `ftn_read_thrown`, `ftn_qb_location`, `ftn_is_qb_out_of_pocket`, the play-action/screen/RPO/trick/no-huddle/motion flags, and pass-rusher/blitzer/box counts. FTN source identifiers are retained with `ftn_` names; `nflverse_game_id` and `nflverse_play_id` are the join keys. A blank contested flag means FTN did not supply a value, not that the target was open.

## Game metadata

`games_2022_onward.csv` contains schedule/game rows, `espn_game_id`, `nflverse_game_id`, teams, date, final score, roof/weather, venue, and rest context. Raw PBP also supplies `roof`, `temp`, `wind`, and `surface` where available. Team labels are for joins/auditing only.

Empty cells mean missing values. Some numeric columns may appear as decimals because nflverse stores them as floating-point fields even when values are whole numbers.
