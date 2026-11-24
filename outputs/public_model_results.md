# Public-model arm-punt analysis

## Method

The primary estimate uses third-down pass attempts with at least 30 air yards, FTN charted contested status or qualifying defensive pass interference, and a pre-play fourth-down filter. For each candidate situation, nfl4th evaluates a hypothetical zero-yard result (4th down at the same line and distance); the situation is retained only when punt is best in more than 50% of 200 game-bootstrap fits. Cutoffs 25, 35, and 40 are reported as sensitivities. Clean pass-outcome probabilities are estimated with a regularized multinomial logistic model using air yards, yards to go, and field position. A separate regularized multinomial model estimates no accepted PI, accepted DPI, and accepted OPI probabilities from target depth, charted-contest status, distance to go, and field position. Its training data combines third-down pass-attempt controls with accepted third-down PI no-play records recovered from full PBP; the PI audit and depth coverage tables document the source coding. The resulting mixture assigns accepted DPI and OPI their own probability mass; clean pass outcomes are weighted by the no-accepted-PI probability. DPI conditional enforcement yards are drawn from observed first-down DPI records matched by target depth and contest status, with local weighting by field position and distance. DPI states are valued as a first down from the enforced spot. OPI follows the pre-declared primary assumption of a declined/equivalent incompletion followed by punt. Interception-return and completion-YAC distributions use the same shallow regression-tree architecture trained on all eligible league pass plays, with target depth and own-yard-line position as predictors; down and distance are omitted from these conditional models. The training observations in each terminal leaf form an empirical outcome distribution, retaining paired interception pick-location offsets and return lengths. Punt values use a separate tree conditioned on punt starting own-yard line. Other outcome states use kernel-weighted empirical conditional distributions from comparable candidate plays. Predicted states are evaluated with public EP and combined with outcome probabilities. All data-trained components are held out by season. No player or team identity predictors are used.

Every predicted possession state is evaluated with nflfastR's public EP model at Q1 15:00, three timeouts, outdoor, using the latest complete season (2025). Punt outcomes are first mapped to offense-relative public EP values; the conditional punt-value distribution is estimated from the terminal leaf of the punt-starting-position tree. For retained candidates, incompletions and offensive penalties are valued as followed by a punt, consistent with the zero-gain fourth-down filter. A non-converting completion is evaluated at its actual fourth-down state with nfl4th; punt is used when it is best in more than half of 200 game-bootstrap fits. Otherwise, public EP at the fourth-down state is the continuation value.

## Results

| Scope | Air-yard cutoff | Eligible | Valued | Mean arm-punt EP | Mean throwaway-punt EP | Mean difference | Game-bootstrap 95% interval |
|---|---:|---:|---:|---:|---:|---:|---:|
| 2022_2026_including_partial | 25 | 279 | 279 | 0.515 | -1.077 | 1.591 | [1.214, 1.633] |
| 2022_2026_including_partial | 30 | 194 | 194 | 0.590 | -1.069 | 1.659 | [1.174, 1.786] |
| 2022_2026_including_partial | 35 | 137 | 137 | 0.575 | -1.069 | 1.644 | [1.187, 1.754] |
| 2022_2026_including_partial | 40 | 92 | 92 | 0.556 | -1.032 | 1.588 | [0.946, 1.741] |
| completed_2022_2025 | 25 | 222 | 222 | 0.596 | -1.096 | 1.693 | [1.280, 1.836] |
| completed_2022_2025 | 30 | 153 | 153 | 0.648 | -1.093 | 1.741 | [1.275, 1.850] |
| completed_2022_2025 | 35 | 105 | 105 | 0.648 | -1.090 | 1.738 | [1.138, 1.885] |
| completed_2022_2025 | 40 | 69 | 69 | 0.574 | -1.030 | 1.604 | [0.891, 1.902] |

## Throwaways in the qualifying screen

The strict primary 30-air-yard sample contains 194 charted-contested or DPI-qualified plays; none was marked as a throwaway by FTN. For a descriptive comparison across 2022–2026 (with 2026 partial), a supplemental screen adds FTN-flagged throwaways at 30 or more recorded air yards and applies the same zero-gain fourth-down punt filter. This expanded screen has 202 plays: 8 FTN-flagged throwaways, 123 regular incompletions, and 131 total incompletions. The eight throwaways came from eight quarterbacks, one each. Passer labels are abbreviated names parsed from play descriptions because the packaged data has no structured passer-name field. These denominators are small and do not establish stable quarterback tendencies.

| Quarterback label | Screened plays | FTN throwaways | Share |
|---|---:|---:|---:|
| C.Williams | 2 | 1 | 50.0% |
| T.Heinicke | 2 | 1 | 50.0% |
| R.Wilson | 4 | 1 | 25.0% |
| S.Howell | 4 | 1 | 25.0% |
| B.Nix | 6 | 1 | 16.7% |
| D.Prescott | 6 | 1 | 16.7% |
| A.Rodgers | 7 | 1 | 14.3% |
| P.Mahomes | 15 | 1 | 6.7% |

> **Suggested future application:** “The FTN throwaway flag separates deliberate disposal from ordinary incompletions. In this expanded screen, eight 30-plus-air-yard, punt-eligible attempts were flagged as throwaways, with one occurrence for each of eight quarterbacks. These plays are leads for reviewing pressure, receiver separation, and film to ask whether a downfield option was available; the data alone do not show that a receiver was open or that a throw would have been preferable.”

Recorded air yards on a throwaway do not necessarily identify an intended target or a viable receiver option. Row-level details and quarterback counts are in `arm_punt_throwaway_situations.csv` and `arm_punt_throwaway_quarterback_counts.csv`.

## Pass-interference model

Accepted PI is recovered from full play-by-play no-play rows rather than the pass-attempt extract. The probability model uses 21,820 no-penalty third-down pass controls, 472 depth-known accepted DPI records, and 86 depth-known accepted OPI records. Four DPI and four OPI records have unknown target depth and are excluded from fitting. The accepted-DPI conditional distribution contains 472 enforced-yard observations (mean 14.92; median 13; 25th/75th percentiles 8/20). Held-out penalty probabilities average 0.110 log loss and 0.047 multiclass Brier score.

| Held-out season | Plays | Observed DPI rate | Predicted DPI rate | Observed OPI rate | Predicted OPI rate |
|---:|---:|---:|---:|---:|---:|
| 2022 | 4921 | 0.017 | 0.017 | 0.004 | 0.004 |
| 2023 | 5143 | 0.021 | 0.022 | 0.003 | 0.004 |
| 2024 | 4869 | 0.023 | 0.022 | 0.003 | 0.004 |
| 2025 | 4799 | 0.022 | 0.022 | 0.005 | 0.004 |
| 2026 | 2646 | 0.023 | 0.024 | 0.004 | 0.004 |

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

Penalty modeling coverage, accepted-PI records, target-depth recovery, and season-held-out penalty probability validation are reported in `penalty_model_training_coverage.csv`, `accepted_pi_training_rows.csv`, `accepted_pi_target_depth_coverage.csv`, `accepted_dpi_distribution_coverage.csv`, and `penalty_model_season_heldout_validation.csv`. Detailed tables are in `outputs/tables/`: strategy results, candidate-level values, combined outcome probabilities, separate penalty probabilities, conditional expected path values, category rates, observed conditional outcome-state summaries, interception-return, completion-YAC, and punt-model coverage, fourth-down decisions, and public-model verification.
