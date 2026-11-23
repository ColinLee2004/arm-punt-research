"""First-pass, reproducible league-wide analysis using only packaged CSVs.

Run from repository root with Python 3.10+; standard library only.
"""
import csv, gzip, json, math, random, statistics
from collections import Counter, defaultdict
from pathlib import Path
from functools import lru_cache

ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "data"
OUT = ROOT / "outputs" / "tables"
OUT.mkdir(parents=True, exist_ok=True)
CUTOFFS = (25, 30, 35, 40)
BOOTSTRAPS = 400

def read_rows(path):
    op = gzip.open if path.suffix == ".gz" else open
    with op(path, "rt", encoding="utf-8-sig", newline="") as f:
        yield from csv.DictReader(f)

def n(row, key):
    try:
        x = float(row.get(key, ""))
        return x if math.isfinite(x) else None
    except (TypeError, ValueError): return None

def yes(row, key): return str(row.get(key, "")).strip().lower() in {"true", "1", "1.0"}
def avg(xs):
    xs = list(xs)
    return statistics.fmean(xs) if xs else None
def quantile(xs, q):
    xs = sorted(x for x in xs if x is not None)
    if not xs: return None
    z=(len(xs)-1)*q; lo=math.floor(z); hi=math.ceil(z)
    return xs[lo]+(xs[hi]-xs[lo])*(z-lo)
def write_csv(path, records):
    records=list(records)
    if records:
        with path.open("w",encoding="utf-8",newline="") as f:
            w=csv.DictWriter(f,fieldnames=list(records[0])); w.writeheader(); w.writerows(records)

def drive_points(d):
    r=(d.get("result") or "").strip().lower()
    return {"touchdown":7.0,"field goal":3.0,"safety":-2.0,"opp touchdown":-7.0}.get(r,0.0)

def drive_key(row):
    fixed=n(row,"fixed_drive")
    return (row.get("game_id"),int(fixed) if fixed is not None else None)

def solve_linear(a,b):
    """Small dense Gaussian elimination with partial pivoting."""
    m=[list(row)+[rhs] for row,rhs in zip(a,b)]; size=len(b)
    for col in range(size):
        pivot=max(range(col,size),key=lambda r:abs(m[r][col]))
        if abs(m[pivot][col])<1e-12: raise ValueError("Singular EP regression matrix")
        m[col],m[pivot]=m[pivot],m[col]
        scale=m[col][col]; m[col]=[x/scale for x in m[col]]
        for r in range(size):
            if r==col: continue
            factor=m[r][col]
            if factor: m[r]=[x-factor*y for x,y in zip(m[r],m[col])]
    return [m[i][-1] for i in range(size)]

class NeutralStateValue:
    """Project-local drive EP model, evaluated at a fixed neutral game context."""
    CONTEXT={"quarter":1.0,"quarter_seconds_remaining":900.0,"score_diff":0.0,
             "offense_timeouts":3.0,"defense_timeouts":3.0}
    RIDGE=50.0
    def __init__(self, drives, play_path, seasons=None):
        if seasons is not None:
            drives=[d for d in drives if drive_season(d) in seasons]
        outcome={drive_key(d):drive_points(d) for d in drives}
        counts=Counter(); near_neutral=Counter()
        # Weight each drive equally so long drives do not dominate the training target.
        for p in read_rows(play_path):
            if seasons is not None and (n(p,"season") is None or int(n(p,"season")) not in seasons): continue
            key=drive_key(p)
            x=self.features(p)
            if key in outcome and x is not None:
                counts[key]+=1
                if (n(p,"qtr")==1 and n(p,"quarter_seconds_remaining") is not None
                    and n(p,"quarter_seconds_remaining")>=840
                    and (n(p,"posteam_score") or 0)==(n(p,"defteam_score") or 0)
                    and n(p,"posteam_timeouts_remaining")==3 and n(p,"defteam_timeouts_remaining")==3):
                    near_neutral[int(n(p,"down"))]+=1
        width=len(self.features_from_values(50,1,10,1,900,0,3,3))
        xtx=[[0.0]*width for _ in range(width)]; xty=[0.0]*width
        nrows=0; total_weight=0.0; observed=[]
        for p in read_rows(play_path):
            if seasons is not None and (n(p,"season") is None or int(n(p,"season")) not in seasons): continue
            key=drive_key(p)
            if key not in outcome or not counts[key]: continue
            x=self.features(p)
            if x is None: continue
            y=outcome[key]; w=1.0/counts[key]
            nrows+=1; total_weight+=w; observed.append((x,y,w))
            for i,xi in enumerate(x):
                xty[i]+=w*xi*y
                for j,xj in enumerate(x): xtx[i][j]+=w*xi*xj
        for i in range(1,width): xtx[i][i]+=self.RIDGE
        self.beta=solve_linear(xtx,xty)
        self.training_rows=nrows; self.training_drives=len(counts); self.training_weight=total_weight
        self.near_neutral_by_down=near_neutral
        # In-sample fit diagnostics, weighted equally by drive.
        sse=sum(w*(sum(a*b for a,b in zip(x,self.beta))-y)**2 for x,y,w in observed)
        self.weighted_rmse=math.sqrt(sse/total_weight) if total_weight else None

    @staticmethod
    def features_from_values(own,down,togo,quarter,quarter_seconds_remaining,score_diff,offense_timeouts,defense_timeouts):
        own=min(100,max(0,float(own)))/100.0
        togo=min(100,max(1,float(togo)))/10.0
        d2=1.0 if int(down)==2 else 0.0; d3=1.0 if int(down)==3 else 0.0
        d4=1.0 if int(down)==4 else 0.0
        q=max(1.0,float(quarter))/4.0; clk=min(900,max(0,float(quarter_seconds_remaining)))/900.0
        score=max(-21,min(21,float(score_diff)))/21.0
        off_to=min(3,max(0,float(offense_timeouts)))/3.0; def_to=min(3,max(0,float(defense_timeouts)))/3.0
        return [1.0,own,own*own,d2,d3,d4,togo,togo*togo,q,clk,clk*clk,score,
                off_to,def_to,own*d2,own*d3,own*d4,own*togo]

    @classmethod
    def features(cls,p):
        yl=n(p,"yardline_100"); down=n(p,"down"); togo=n(p,"ydstogo")
        if yl is None or down is None or togo is None or down not in (1,2,3,4): return None
        q=n(p,"qtr") or 1; clock=n(p,"quarter_seconds_remaining")
        if clock is None: clock=450.0
        score=(n(p,"posteam_score") or 0)-(n(p,"defteam_score") or 0)
        off_to=n(p,"posteam_timeouts_remaining"); def_to=n(p,"defteam_timeouts_remaining")
        return cls.features_from_values(100-yl,down,togo,q,clock,score,
                                        3 if off_to is None else off_to,
                                        3 if def_to is None else def_to)

    def value(self,own_yards,down=1,ydstogo=10):
        if own_yards is None:return None
        f=self.features_from_values(own_yards,down,ydstogo,**self.CONTEXT)
        return sum(a*b for a,b in zip(f,self.beta))

def opponent_value(row, value, expected_team):
    if yes(row,"return_touchdown") and row.get("return_team")==expected_team:
        return -7.0
    if row.get("next_drive_team") != expected_team:
        if yes(row,"next_drive_is_score") and (row.get("next_drive_result") or "").lower()=="touchdown": return -7.0
        return None
    own=n(row,"next_drive_own_yardline")
    v=state_ep(value,own)
    return -v if v is not None else None

def state_ep(value,own,down=1,ydstogo=None):
    if own is None:return None
    if own>=100:return 7.0
    if ydstogo is None: ydstogo=max(1,min(10,100-own))
    return value.value(own,down,ydstogo)

def punt_net_value(row,value):
    if yes(row,"return_touchdown"):
        if row.get("return_team")==row.get("defteam"): return -7.0
        if row.get("return_team")==row.get("posteam"): return 7.0
    team=row.get("next_drive_team"); own=n(row,"next_drive_own_yardline")
    if team==row.get("defteam"):
        v=state_ep(value,own); return -v if v is not None else None
    if team==row.get("posteam"):
        return state_ep(value,own)
    return None

_PUNT_CACHE = {}
def punt_value_at(own_spot, punts, value, seasons=None):
    if own_spot is None: return None,0
    season_key=tuple(sorted(seasons)) if seasons is not None else None
    key=(id(punts),id(value),season_key)
    if key not in _PUNT_CACHE:
        buckets=defaultdict(list)
        for p in punts:
            if seasons is not None and int(float(p["season"])) not in seasons: continue
            yl=n(p,"yardline_100")
            if yl is None: continue
            v=punt_net_value(p,value)
            if v is not None: buckets[int(round(100-yl))].append(v)
        _PUNT_CACHE[key]=buckets
    buckets=_PUNT_CACHE[key]
    for radius in (7.5,15,25,101):
        here=[v for spot,vals in buckets.items() if abs(spot-own_spot)<=radius for v in vals]
        if len(here)>=20 or radius==101: return avg(here),len(here)
    return None,0

def classify_dpi(p):
    return ("pass interference" in (p.get("penalty_type") or "").lower()
            and p.get("penalty_team")==p.get("defteam") and yes(p,"first_down_penalty"))

def pass_value(p,value,punts):
    offense,defense=p.get("posteam"),p.get("defteam")
    yl=n(p,"yardline_100"); own=100-yl if yl is not None else None
    if yes(p,"interception") or (yes(p,"fumble_lost") and p.get("next_drive_team")==defense):
        return opponent_value(p,value,defense),"turnover"
    if classify_dpi(p):
        spot=min(100,max(0,(own or 0)+(n(p,"penalty_yards") or 0)))
        return state_ep(value,spot),"defensive_pass_interference"
    if p.get("penalty_team")==offense and p.get("penalty_type"):
        v,_=punt_value_at(own,punts,value); return v,"offensive_penalty_as_incomplete"
    if yes(p,"complete_pass"):
        gain=n(p,"yards_gained")
        if gain is None or own is None: return None,"completion_missing_spot"
        spot=min(100,max(0,own+gain))
        if own+gain>=100:return 7.0,"completion_touchdown"
        converts=yes(p,"first_down_pass") or (n(p,"ydstogo") is not None and gain>=n(p,"ydstogo"))
        if converts: return state_ep(value,spot),"completion_converts"
        v,_=punt_value_at(spot,punts,value); return v,"completion_short_then_punt"
    if yes(p,"incomplete_pass"):
        v,_=punt_value_at(own,punts,value); return v,"incomplete_then_punt"
    return None,"other_or_unresolved"

def bootstrap_ci(items):
    by=defaultdict(list)
    for game,x in items: by[game].append(x)
    games=list(by)
    if not games:return None,None
    rng=random.Random(20261121+len(items)); reps=[]
    for _ in range(BOOTSTRAPS):
        chosen=[rng.choice(games) for _ in games]
        vals=[x for g in chosen for x in by[g]]
        reps.append(avg(vals))
    return quantile(reps,.025),quantile(reps,.975)

def eligible(p, cutoff):
    air=n(p,"air_yards")
    return (air is not None and air>=cutoff and yes(p,"pass_attempt") and not yes(p,"sack")
            and not yes(p,"qb_scramble") and (yes(p,"ftn_is_contested_ball") or classify_dpi(p)))

def drive_season(d):
    # The packaged drive CSV uses game_id (YYYY_week_away_home) rather than season.
    try: return int(str(d.get("game_id", ""))[:4])
    except ValueError: return None

def main():
    manifest=json.loads((DATA/"manifest.json").read_text(encoding="utf-8"))
    passes=list(read_rows(DATA/"third_down_passes_2022_onward.csv"))
    punts=list(read_rows(DATA/"punt_plays_2022_onward.csv"))
    drives=list(read_rows(DATA/"drives_2022_onward.csv"))
    pbp_path=DATA/"plays_2022_onward.csv.gz"
    value=NeutralStateValue(drives,pbp_path)
    audit=[{"metric":"third_down_pass_rows","value":len(passes)},
           {"metric":"punt_rows","value":len(punts)},{"metric":"drive_rows","value":len(drives)},
           {"metric":"ftn_contested_true","value":sum(yes(p,"ftn_is_contested_ball") for p in passes)},
           {"metric":"ftn_contested_false","value":sum(str(p.get("ftn_is_contested_ball")).lower()=="false" for p in passes)},
           {"metric":"ftn_contested_missing","value":sum(not str(p.get("ftn_is_contested_ball","")).strip() for p in passes)},
           {"metric":"manifest_full_pbp_rows","value":manifest["files"]["plays_2022_onward.csv.gz"]["rows"]},
           {"metric":"manifest_ftn_joined_rows","value":manifest.get("ftn_charting_rows_joined","")}]
    write_csv(OUT/"data_audit.csv",audit)
    state=[]
    for y in range(0,101,5):
        distance=max(1,min(10,100-y))
        state.append({"own_yardline":y,"down":1,"yards_to_go":distance,
                      "neutral_ep":round(value.value(y,1,distance),5)})
    write_csv(OUT/"state_value_by_field_position.csv",state)
    write_csv(OUT/"state_model_diagnostics.csv",[{
        "training_play_states":value.training_rows,"training_drives":value.training_drives,
        "equal_drive_weight_total":round(value.training_weight,2),"weighted_in_sample_rmse":round(value.weighted_rmse,4),
        "ridge_penalty":value.RIDGE,"context":"Q1 15:00, tied 0-0, 3 timeouts per side",
        "predictors":"possession-relative field position, down, yards to go, quarter, clock, score differential, offense/defense timeouts",
        "actual_q1_tied_states_within_first_60_seconds_by_down":json.dumps(value.near_neutral_by_down,sort_keys=True)}])

    result_rows=[]; candidate_rows=[]; valued_rows=[]
    for cutoff in CUTOFFS:
        cand=[p for p in passes if eligible(p,cutoff)]; items=[]; branches=Counter(); unresolved=Counter()
        for p in cand:
            pv,branch=pass_value(p,value,punts); branches[branch]+=1
            yl=n(p,"yardline_100"); own=100-yl if yl is not None else None
            base,nb=punt_value_at(own,punts,value)
            if pv is None or base is None: unresolved[branch]+=1; continue
            delta=pv-base; items.append((p.get("game_id",""),delta))
            valued_rows.append({"cutoff_air_yards":cutoff,"game_id":p.get("game_id"),"season":p.get("season"),
                "branch":branch,"candidate_value_net_points":round(pv,6),"throwaway_punt_value":round(base,6),
                "delta_net_points":round(delta,6),"punt_comparator_n":nb})
        lo,hi=bootstrap_ci(items)
        result_rows.append({"cutoff_air_yards":cutoff,"eligible_n":len(cand),"valued_n":len(items),
            "unvalued_n":len(cand)-len(items),"mean_delta_net_points":round(avg(x for _,x in items),4) if items else "",
            "game_bootstrap_95ci_low":round(lo,4) if lo is not None else "",
            "game_bootstrap_95ci_high":round(hi,4) if hi is not None else "",
            "game_clusters":len(set(g for g,_ in items)),"positive_delta_share":round(sum(x>0 for _,x in items)/len(items),4) if items else "",
            "unvalued_branch_counts":json.dumps(unresolved,sort_keys=True)})
        candidate_rows.append({"cutoff_air_yards":cutoff,"eligible_n":len(cand),"valued_n":len(items),
            **{"n_"+k:v for k,v in branches.items()},
            **({"pct_"+k:round(v/len(cand),4) for k,v in branches.items()} if cand else {})})
    write_csv(OUT/"cutoff_sensitivity.csv",result_rows)
    write_csv(OUT/"candidate_outcomes_by_cutoff.csv",candidate_rows)
    write_csv(OUT/"valued_candidate_plays.csv",valued_rows)

    # Descriptive league punt baseline and receiving-side starting position.
    punt_rows=[]
    punt_flags={"blocked":"punt_blocked","fair_catch":"punt_fair_catch","downed":"punt_downed",
                "out_of_bounds":"punt_out_of_bounds","touchback":"touchback","inside_twenty":"punt_inside_twenty",
                "in_endzone":"punt_in_endzone"}
    for label,flag in punt_flags.items():
        punt_rows.append({"metric":label,"n_true":sum(yes(p,flag) for p in punts),"punt_attempts":len(punts),
                          "pct":round(sum(yes(p,flag) for p in punts)/len(punts),4) if punts else ""})
    return_obs=[p for p in punts if p.get("next_drive_team")==p.get("defteam") and n(p,"next_drive_own_yardline") is not None]
    recpos=[n(p,"next_drive_own_yardline") for p in return_obs]
    punt_rows += [{"metric":"valid_receiving_drive_start_n","n_true":len(return_obs),"punt_attempts":len(punts),"pct":round(len(return_obs)/len(punts),4)},
                  {"metric":"receiving_start_own_yards_mean","n_true":"","punt_attempts":len(return_obs),"pct":round(avg(recpos),3)},
                  {"metric":"receiving_start_own_yards_median","n_true":"","punt_attempts":len(return_obs),"pct":round(quantile(recpos,.5),3)},
                  {"metric":"receiving_start_own_yards_q25","n_true":"","punt_attempts":len(return_obs),"pct":round(quantile(recpos,.25),3)},
                  {"metric":"receiving_start_own_yards_q75","n_true":"","punt_attempts":len(return_obs),"pct":round(quantile(recpos,.75),3)}]
    valued_punts=sum(punt_net_value(p,value) is not None for p in punts)
    punt_rows += [{"metric":"punt_outcomes_with_neutral_ep_value","n_true":valued_punts,"punt_attempts":len(punts),"pct":round(valued_punts/len(punts),4)},
                  {"metric":"punt_outcomes_without_neutral_ep_value","n_true":len(punts)-valued_punts,"punt_attempts":len(punts),"pct":round((len(punts)-valued_punts)/len(punts),4)}]
    write_csv(OUT/"punt_descriptives.csv",punt_rows)

    complete={2022,2023,2024,2025}; compdr=[d for d in drives if drive_season(d) in complete]
    comppu=[p for p in punts if int(float(p["season"])) in complete]
    season_rows=[]
    for name,ps,ds,pu in [("2022-2026_including_partial",passes,drives,punts),
                           ("completed_2022-2025",[p for p in passes if int(float(p["season"])) in complete],compdr,comppu)]:
        dv=value if name=="2022-2026_including_partial" else NeutralStateValue(ds,pbp_path,complete)
        sample=[]; total=0
        for p in ps:
            if not eligible(p,30):continue
            total+=1; pv,_=pass_value(p,dv,pu); yl=n(p,"yardline_100")
            base,_=punt_value_at(100-yl if yl is not None else None,pu,dv,complete if name=="completed_2022-2025" else None)
            if pv is not None and base is not None:sample.append((p.get("game_id",""),pv-base))
        lo,hi=bootstrap_ci(sample)
        season_rows.append({"scope":name,"eligible_n":total,"valued_n":len(sample),
            "mean_delta_net_points":round(avg(x for _,x in sample),4) if sample else "",
            "game_bootstrap_95ci_low":round(lo,4) if lo is not None else "",
            "game_bootstrap_95ci_high":round(hi,4) if hi is not None else ""})
    write_csv(OUT/"season_scope_sensitivity.csv",season_rows)

    lines=["# Initial arm-punt research results","",
      "## Design","",
      "Candidates are third-down pass attempts with air yards at or beyond the stated cutoff, charted contested by FTN or flagged as an apparent accepted defensive pass-interference penalty (defense assessed and first-down flag). The primary cutoff is 30 yards; 25, 35, and 40 are reported as requested. This retrospective class does not observe the quarterback's private expectation.","",
      "The throwaway comparator is a guaranteed incompletion and punt from the same field position. Its value is the local pooled punt outcome distribution's expected negative net points for the receiving drive. Drive value is a transparent smoothed average of net points (TD +7, field goal +3, safety -2, opponent return TD -7, otherwise 0) by starting own-yard line. A converting completion uses that value at its resulting spot; a short completion is forced to punt; a turnover uses the recorded next-drive possession and location; an offensive penalty is treated as incompletion plus punt. No team/player identity effects are included.","",
      "## Cutoff results","",
      "Intervals resample games while holding the fitted drive-value curve and punt matching procedure fixed. They do not include uncertainty from refitting all model components.","",
      "| Air yards | Eligible | Valued | Mean net-point difference | Game-bootstrap 95% interval | Positive-difference share |","|---:|---:|---:|---:|---:|---:|"]
    for r in result_rows: lines.append(f"| {r['cutoff_air_yards']} | {r['eligible_n']} | {r['valued_n']} | {r['mean_delta_net_points']} | [{r['game_bootstrap_95ci_low']}, {r['game_bootstrap_95ci_high']}] | {r['positive_delta_share']} |")
    maincats=next(r for r in candidate_rows if r["cutoff_air_yards"]==30)
    lines += ["","At the 30-yard cutoff, the observed branches were: " + "; ".join(
      f"{k[2:]}: {v} ({maincats.get('pct_'+k[2:],0):.1%})" for k,v in maincats.items() if k.startswith("n_")) + ".",
      "","With only completed 2022-2025 seasons, the 30-yard estimate was " +
      f"{season_rows[1]['mean_delta_net_points']} net points (95% game-bootstrap interval " +
      f"{season_rows[1]['game_bootstrap_95ci_low']} to {season_rows[1]['game_bootstrap_95ci_high']}; " +
      f"{season_rows[1]['valued_n']} valued candidates).",
      "","## Limitations","",
      "This is a first-pass comparison on a common pooled value scale, not a validated play-calling rule. Drive outcomes pool different real score and clock contexts; they are not standardized to the planned canonical Q1 15:00, 0-0 scenario, nor does the value function explicitly value down-and-distance continuation states. Observed throws are selected by play design, quarterback decisions, and defense, so the comparison is not causal. The punt model is pooled and local by starting position, but special cases such as blocks, enforcement, return fumbles, and unusual possession sequences need a dedicated audit. DPI identification is intentionally narrow. 2026 is partial. No precise universal break-even threshold should be inferred from this first pass.","",
      "The completed-season sensitivity (2022-2025) is in `season_scope_sensitivity.csv`; pooled punt outcome counts and receiving drive-start position summaries are in `punt_descriptives.csv`. Candidate branch counts/rates are in `candidate_outcomes_by_cutoff.csv`. All tables are under `outputs/tables/`; candidate-level values are in `valued_candidate_plays.csv`.",""]
    (ROOT/"outputs"/"initial_results.md").write_text("\n".join(lines),encoding="utf-8")
    print("Wrote outputs/initial_results.md and outputs/tables/*.csv")
    for r in result_rows: print(r)

if __name__=="__main__": main()
