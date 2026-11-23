# Arm Punt Research Data

A portable, team-agnostic NFL play-by-play data package for the arm-punt versus throwaway study. The CSVs can be analyzed without access to the `nba-pbp` repository or its RDS database.

## Sources and coverage

Play-by-play is from nflverse's raw public PBP files; pass charting is from FTN's public charting feed distributed by nflverse ([data repository](https://github.com/nflverse/nflverse-data), [FTN charting releases](https://github.com/nflverse/nflverse-data/releases/tag/ftn_charting)). Seasons 2022–2025 are complete; 2026 is partial through the export date in `data/manifest.json`.

| File | Rows | Use |
|---|---:|---|
| `data/plays_2022_onward.csv.gz` | 223,756 | Full selected nflverse play-by-play, FTN charting, current/next drive context, and game context. Compressed CSV; common data tools read it directly. |
| `data/third_down_passes_2022_onward.csv` | 24,647 | Third-down pass attempts for candidate definition and outcome analysis. |
| `data/punt_plays_2022_onward.csv` | 9,966 | Punt attempts and recorded punt/return outcomes. |
| `data/drives_2022_onward.csv` | 28,075 | Drive starts/results and the following drive's start. |
| `data/games_2022_onward.csv` | 1,415 | Game dates, score, roof/weather, venue, and schedule crosswalk. |

`data/manifest.json` records extraction time, row/column counts, byte sizes, and SHA-256 hashes.

## Modeling scope

This is a league-wide experiment. Team and player labels are retained for joins, auditing, and game-clustered resampling only. Do not include team, quarterback, receiver, punter, or return-unit identity effects in the primary models. Punt outcomes represent the pooled NFL distribution.

For the initial comparison, standardize both choices to a 0-0 score with plenty of time remaining. A third-down situation qualifies as an arm-punt opportunity only when the public `nfl4th` model recommends a punt on the hypothetical fourth down after zero yards, in more than 50% of its 200 bootstrap draws. Thus, retained incompletions and throwaways are followed by a punt. Later analyses can vary score and time.

The selected valuation workflow uses the public `nflfastR` EP model for every simulated resulting state and the public `nfl4th` model both to filter candidate situations based on the zero-gain fourth-down state and to evaluate short-completion fourth-down decisions. Outcome-category probabilities and conditional state distributions are estimated as separate components; their combination is then valued with public EP. A fourth-down state is treated as a punt situation when punt is the best nfl4th option in more than half of its bootstrap draws. `punt_wp` alone is not a punt probability.

## Data conventions

- Empty CSV cells represent missing values.
- The main play file is based on nflverse raw PBP, not the narrower normalized RDS play table. It includes yards to go, quarter and clock, EP/EPA, timeouts, and detailed punt flags.
- `third_down_passes_2022_onward.csv` filters the full play file to `down = 3` and `pass_attempt = 1`. It includes charted and uncharted plays; missing FTN fields do not mean a play was uncontested.
- `punt_plays_2022_onward.csv` filters to `punt_attempt = 1`. Use the full play file to inspect related event rows such as return touchdowns or other possession changes.
- `game_id` is nflverse's game key; `espn_game_id` is the matching ESPN key. `nflverse_play_id` is the nflverse play number. FTN fields are joined by nflverse game/play ID and prefixed `ftn_`.
- `yardline_100` and drive `start_yardline` are distances from the possessing offense's opponent goal line. For example, `80` means its own 20. Convert with `100 - yardline_100` for own-yard-line coordinates.
- `air_yards` is target depth; `yards_gained` is total play gain. Use total gain to locate a completion's resulting spot.
- `return_yards` is generic; `interception_return_yards` and `punt_return_yards` are derived splits.
- `next_drive_start_yardline` and `next_drive_own_yardline` are observed next-drive starts, not modeled counterfactuals. Check `next_drive_team` and the play result because scores, blocks, or unusual sequences can alter which team has the next drive.
- `ep`, `epa`, `wp`, and related fields are source-model outputs under actual play context. Revalue states under the fixed neutral baseline rather than treating actual-context EPA as already standardized.
- `ftn_is_contested_ball` is retrospective charting, not a direct record of the quarterback's private expectation.

See [`DATA_DICTIONARY.md`](DATA_DICTIONARY.md) and [`data/manifest.json`](data/manifest.json).

## Reproduce the analysis

The selected analysis uses `scripts/run_public_analysis.R` and writes its report to `outputs/public_model_results.md`, with result tables in `outputs/tables/`. It uses the public `nflfastR` EP model, the public `nfl4th` fourth-down model and its game-bootstrap fits, plus separately estimated outcome probabilities and conditional outcome-state distributions. It reads only the packaged CSVs.

Install R and run the setup script once:

```powershell
& 'C:\Program Files\R\R-4.6.2\bin\Rscript.exe' scripts/setup_environment.R
```

From a PowerShell session with outbound access, cache the public model data once:

```powershell
& 'C:\Program Files\R\R-4.6.2\bin\Rscript.exe' scripts/cache_public_models.R
```

Then run the analysis from the repository root:

```powershell
& 'C:\Program Files\R\R-4.6.2\bin\Rscript.exe' scripts/run_public_analysis.R
```

To score a specific third-down situation with the primary model (30+ air yards), supply target depth, yards to go, and the offense's own-yard-line position:

```powershell
& 'C:\Program Files\R\R-4.6.2\bin\Rscript.exe' scripts/predict_arm_punt.R --air-yards 35 --yards-to-go 8 --own-yardline 42
```

The predictor prints the predicted outcome probabilities, conditional EP by outcome, arm-punt EP, throwaway-and-punt EP, and their difference. Its predictors are limited to those three inputs; score, clock, weather, and team are held fixed or unused under the neutral league-wide setup. Use `--help` to see the input format.

R package libraries and public-model caches are stored in project-local `.r-library/` and `.r-cache/` directories. The cache script needs network access; subsequent analysis runs use those local files.

## Exploratory first pass

The existing `scripts/run_analysis.py` and `outputs/initial_results.md` are an exploratory first pass only. They use a project-local EP curve and do not implement the selected public-model/distribution workflow. Do not treat their estimates as the planned analysis.

The exploratory first pass uses a 30-air-yard primary cutoff and sensitivity cutoffs of 25, 35, and 40 yards. Its tables can be reproduced from the repository root with:

```powershell
python scripts/run_analysis.py
```

The script uses Python's standard library and reads only the packaged CSVs. Results are written to `outputs/`; see [`outputs/initial_results.md`](outputs/initial_results.md). This first pass uses a project-local drive outcome value by starting field position and is explicitly exploratory; it does not implement the selected public-model workflow.
