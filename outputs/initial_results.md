# Initial arm-punt research results

## Design

Candidates are third-down pass attempts with air yards at or beyond the stated cutoff, charted contested by FTN or flagged as an apparent accepted defensive pass-interference penalty (defense assessed and first-down flag). The primary cutoff is 30 yards; 25, 35, and 40 are reported as requested. This retrospective class does not observe the quarterback's private expectation.

The throwaway comparator is a guaranteed incompletion and punt from the same field position. Its value is the local pooled punt outcome distribution's expected negative net points for the receiving drive. Drive value is a transparent smoothed average of net points (TD +7, field goal +3, safety -2, opponent return TD -7, otherwise 0) by starting own-yard line. A converting completion uses that value at its resulting spot; a short completion is forced to punt; a turnover uses the recorded next-drive possession and location; an offensive penalty is treated as incompletion plus punt. No team/player identity effects are included.

## Cutoff results

Intervals resample games while holding the fitted drive-value curve and punt matching procedure fixed. They do not include uncertainty from refitting all model components.

| Air yards | Eligible | Valued | Mean net-point difference | Game-bootstrap 95% interval | Positive-difference share |
|---:|---:|---:|---:|---:|---:|
| 25 | 489 | 485 | 1.7671 | [1.5331, 2.0167] | 0.3052 |
| 30 | 294 | 290 | 1.7695 | [1.4609, 2.0919] | 0.2966 |
| 35 | 187 | 183 | 1.7985 | [1.358, 2.2175] | 0.2896 |
| 40 | 118 | 115 | 1.6832 | [1.155, 2.244] | 0.2696 |

At the 30-yard cutoff, the observed branches were: turnover: 18 (6.1%); completion_converts: 70 (23.8%); incomplete_then_punt: 192 (65.3%); completion_touchdown: 13 (4.4%); offensive_penalty_as_incomplete: 1 (0.3%).

With only completed 2022-2025 seasons, the 30-yard estimate was 1.8623 net points (95% game-bootstrap interval 1.4722 to 2.2428; 234 valued candidates).

## Limitations

This is a first-pass comparison on a common pooled value scale, not a validated play-calling rule. Drive outcomes pool different real score and clock contexts; they are not standardized to the planned canonical Q1 15:00, 0-0 scenario, nor does the value function explicitly value down-and-distance continuation states. Observed throws are selected by play design, quarterback decisions, and defense, so the comparison is not causal. The punt model is pooled and local by starting position, but special cases such as blocks, enforcement, return fumbles, and unusual possession sequences need a dedicated audit. DPI identification is intentionally narrow. 2026 is partial. No precise universal break-even threshold should be inferred from this first pass.

The completed-season sensitivity (2022-2025) is in `season_scope_sensitivity.csv`; pooled punt outcome counts and receiving drive-start position summaries are in `punt_descriptives.csv`. Candidate branch counts/rates are in `candidate_outcomes_by_cutoff.csv`. All tables are under `outputs/tables/`; candidate-level values are in `valued_candidate_plays.csv`.
