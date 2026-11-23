# Public-model arm-punt analysis

## Method

The primary estimate uses third-down pass attempts with at least 30 air yards, FTN charted contested status or qualifying defensive pass interference, and a pre-play fourth-down filter. For each candidate situation, nfl4th evaluates a hypothetical zero-yard result (4th down at the same line and distance); the situation is retained only when punt is best in more than 50% of 200 game-bootstrap fits. Cutoffs 25, 35, and 40 are reported as sensitivities. Outcome probabilities are fit with a regularized multinomial logistic model using only air yards, yards to go, and field position. Interception-return and completion-YAC distributions use the same shallow regression-tree architecture trained on all eligible league pass plays, with target depth and own-yard-line position as predictors; down and distance are omitted from these conditional models. The training observations in each terminal leaf form an empirical outcome distribution, retaining paired interception pick-location offsets and return lengths. Punt values use a separate tree conditioned on punt starting own-yard line. Other outcome states use kernel-weighted empirical conditional distributions from comparable candidate plays. Predicted states are evaluated with public EP and combined with outcome probabilities. All data-trained components are held out by season. No player or team identity predictors are used.

Every predicted possession state is evaluated with nflfastR's public EP model at Q1 15:00, three timeouts, outdoor, using the latest complete season (2025). Punt outcomes are first mapped to offense-relative public EP values; the conditional punt-value distribution is estimated from the terminal leaf of the punt-starting-position tree. For retained candidates, incompletions and offensive penalties are valued as followed by a punt, consistent with the zero-gain fourth-down filter. A non-converting completion is evaluated at its actual fourth-down state with nfl4th; punt is used when it is best in more than half of 200 game-bootstrap fits. Otherwise, public EP at the fourth-down state is the continuation value.

## Results

| Scope | Air-yard cutoff | Eligible | Valued | Mean arm-punt EP | Mean throwaway-punt EP | Mean difference | Game-bootstrap 95% interval |
|---|---:|---:|---:|---:|---:|---:|---:|
| 2022_2026_including_partial | 25 | 279 | 279 | 0.224 | -1.077 | 1.301 | [1.075, 1.516] |
| 2022_2026_including_partial | 30 | 194 | 194 | 0.311 | -1.069 | 1.380 | [1.009, 1.644] |
| 2022_2026_including_partial | 35 | 137 | 137 | 0.296 | -1.069 | 1.365 | [0.987, 1.630] |
| 2022_2026_including_partial | 40 | 92 | 92 | 0.275 | -1.032 | 1.307 | [0.787, 1.642] |
| completed_2022_2025 | 25 | 222 | 222 | 0.331 | -1.096 | 1.427 | [1.160, 1.735] |
| completed_2022_2025 | 30 | 153 | 153 | 0.392 | -1.093 | 1.484 | [1.132, 1.752] |
| completed_2022_2025 | 35 | 105 | 105 | 0.391 | -1.090 | 1.481 | [1.007, 1.784] |
| completed_2022_2025 | 40 | 69 | 69 | 0.306 | -1.030 | 1.336 | [0.780, 1.857] |

## Season-held-out validation

Outcome probabilities and conditional state distributions were fit on the other seasons and used to predict the held-out season. Average held-out log loss and multiclass Brier score are:

| Air-yard cutoff | Mean log loss | Mean multiclass Brier |
|---:|---:|---:|
| 25 | 0.987 | 0.555 |
| 30 | 0.986 | 0.540 |
| 35 | 1.017 | 0.540 |
| 40 | 0.974 | 0.516 |

## Limits

The observed arm-punt sample is selected and small, so these estimates are predictive standardized associations, not causal effects. Point estimates are held-out predictions: each play's season was excluded from fitting its outcome-probability, interception-return, completion-YAC, and punt-value models. The 100-replicate game bootstrap resamples training games and evaluates out-of-bag games; all data-trained conditional trees are refit on in-bag plays/punts, while public EP and nfl4th remain fixed. There were no observed non-converting completions at the analyzed 25-yard-or-deeper cutoffs, so nfl4th's >50% punt rule did not change those strategy estimates; its neutral 4th-and-5 bootstrap check is in `public_model_verification.csv`.

Detailed tables are in `outputs/tables/`: strategy results, candidate-level values, outcome probabilities, conditional expected path values, category rates, observed conditional outcome-state summaries, interception-return, completion-YAC, and punt-model coverage, fourth-down decisions, and public-model verification.
