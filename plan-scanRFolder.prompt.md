Plan: Scan and remediate R functions

Objectives
- Summarize key defects and risks across `R/`.
- Call out consistency/import issues and style concerns.
- Propose targeted fixes and ask whether to implement patches.
- Clarify the “Enable Claude Haiku 4.5” request (environment/config location needed).

Scope
- Files: All under `R/` in `rhutils`.
- Focus: correctness, runtime errors, namespace/import hygiene, file/path handling, regex patterns, data.table usage, return semantics.

Current TODOs
- Read all R source files: completed.
- Identify defects and risks: completed.
- Summarize findings: in-progress.
- Propose targeted fixes: pending.

High-severity issues (likely runtime errors)
- `R/cal_eval.R`
  - Uses `rhutils::get_basin_daily(out_dir)` with undefined `out_dir` when `Qsim` is a path. Fix by deriving directory from `Qsim` or adding an explicit `out_dir` parameter.
  - Daily eval uses bare `streamflow` inside `[`; prefer explicit column selection.
- `R/cal_defpar_sens.R`
  - `nacol` referenced even when no character-to-numeric conversion occurs; initialize `nacol <- rep(FALSE, ncol(defpar_df_t))` before conversion logic.
- `R/calibration_utils.R`
  - `def_changes_by_evaldf()`: incorrect column exclusion `names(defpar_df) != c(...)`; use `%in%` and logical negation.
  - `def_changes_by_eval()`: only defines `chgvars_stat` for `stat == "NSE"`; add handling for other stats or stop early.
- `R/build_redefine3.R`
  - Uses `stop(cat(...))`; `cat()` returns `NULL`. Replace with `stop("...")`. Also minor typo: “retuned” → “returned”.
- `R/collect_output.R`
  - Path checks/moves mix `output_dir` and `source_dir`; ensure all filesystem operations use `file.path(source_dir, output_dir, ...)` and check existence accordingly.
  - Regex patterns like `"*\\.csv"` invalid; use `".*\\.csv$"`.
- `R/cleanup_utils.R`
  - Invalid patterns (`"*\\.csv"`, `"*\\.params"`); replace with `".*\\.csv$"`, `".*\\.params$"`.
- `R/data_utils.R`
  - `agg_YM()` and `agg_YM_strata()` reference `vars` out of scope; add `vars` as parameter or derive columns internally.
- `defpars_utils.R` (`pars_sens_output_tables()`)
  - Iterates over `sensout` but argument name is `pars_sens_out`; align variable names.
- `R/spinup_utils.R`
  - `world_add_level_i()` uses nonstandard data.table join form; prefer a mapping table and a keyed join, or simple `factor`/`recode` approach.
  - Uses `shift()` without namespace; call `data.table::shift`.
- `R/worldfile_utils.R`
  - `world_add_var()` uses `shift()`; ensure namespaced and consistent data.table usage during split/merge.
- `R/check_worldfile.R`
  - `rownames(psizesdf) = seq_along(psizesdf$area)` references non-existent `area` column; use `seq_len(nrow(psizesdf))`.
- `R/utils.R`
  - `find_runID()` uses `list.files(out_dir, ".csv")`; use `pattern = ".*\\.csv$"`. `sim_alert()` depends on `beepr`; ensure dependency or guard.

Consistency and robustness fixes
- Replace `require()` calls inside functions with Imports in `DESCRIPTION` and proper `NAMESPACE` entries; or ensure namespaced calls (e.g., `data.table::fread`).
- Use `file.path()` consistently for filesystem operations.
- Standardize calibration stat naming (use `R2` consistently).
- Ensure analytical functions return data objects as last expression; limit unsolicited printing.
- Add guards for date clipping functions when no full water-year boundaries exist.
- Whitebox helpers: ensure `whitebox` is declared dependency and functions are namespaced or imported.

Targeted fix proposals (edits to apply)
1) `R/cal_eval.R`
- Accept explicit `out_dir` or derive from `Qsim` when char path; avoid undefined `out_dir`.
- Quote column names in subsetting for clarity.

2) `R/cal_defpar_sens.R`
- Initialize `nacol <- rep(FALSE, ncol(defpar_df_t))` prior to character conversion block; keep logic intact.

3) `R/calibration_utils.R`
- Use `defpar_df_pars <- defpar_df[, !(names(defpar_df) %in% c("Variable","Def_file"))]`.
- Either implement other `stat` branches or `stop("Unsupported stat; only 'NSE' currently implemented")`.

4) `R/build_redefine3.R`
- Replace all `stop(cat(...))` with `stop("...")`; fix message typo.

5) Paths and patterns
- In `R/collect_output.R`: validate with `dir.exists(file.path(source_dir, output_dir))`; update all `list.files()` to use `path = file.path(source_dir, output_dir)`; patterns `".*\\.csv$"`.
- In `R/cleanup_utils.R` and `R/utils.R`: use valid anchored patterns `".*\\.csv$"`, `".*\\.params$"`.

6) `R/data_utils.R`
- Update signatures of `agg_YM()` and `agg_YM_strata()` to accept `vars`; mirror `agg_dyn()` pattern.

7) `defpars_utils.R`
- Fix `pars_sens_output_tables()` to iterate over `pars_sens_out` and use its name vector for titles.

Optional cleanups
- Namespaced calls for `data.table` and `terra` throughout.
- Add missing dependencies in `DESCRIPTION` (`hydroGOF`, `zoo`, `whitebox`, `beepr`, `knitr`).
- Standardize stat naming and return semantics.

Decision points
- Apply high-severity patches now across the listed files?
- Broaden `calibration_utils` stat support vs. constrain with clear error?
- Namespace vs. NAMESPACE-import approach (consistency preference)?

Clarification needed
- "Enable Claude Haiku 4.5 for all clients" is an environment/config change outside this repo. Provide the target system (API gateway, editor settings, CI, backend) to propose concrete steps.
