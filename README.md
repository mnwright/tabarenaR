# tabarenaR

**TabArena evaluation entirely in R.** Give the package any R model as a
`train()` / `predict()` pair and it runs the official
[TabArena](https://tabarena.ai) protocol on the 51 TabArena-v0.1 datasets,
downloads the published results of every method on the leaderboard, and tells
you the Elo (with 95% interval), normalized score, mean rank, harmonic rank,
improvability, win rate and timing columns of the leaderboard, computed the way
the official leaderboard computes them. No Python, no `tabarena`/`tabrepo`
install: the only hard dependencies are `jsonlite` and `nanoparquet`.

The package reproduces the online leaderboard to rounding precision: recomputing
the current "models + open-source systems, no imputation" board from the live
per-method results gives a maximum Elo difference of 0.53 over 86 methods
(the website rounds Elo to integers), and identical normalized scores, ranks
and harmonic ranks to their displayed precision
(`data-raw/validate_against_website.R`).

## Installation

```r
# from the package directory
install.packages("tabarenaR", repos = NULL, type = "source")
# or, with the source tree checked out:
# remotes::install_local("tabarenaR")
```

Optional: `arrow` (needed to read a handful of the live result files that
carry a nested column), `foreign` (ARFF fallback when OpenML has no parquet
file), `nnet` / `ranger` for the examples.

## Quick start

```r
library(tabarenaR)

# 1. Wrap any model. train(X, y, ctx) returns a fitted object,
#    predict(model, X, ctx) returns predictions (see ?ta_learner for the format).
rf <- ta_learner(
  name = "ranger (R)",
  train = function(X, y, ctx) {
    ranger::ranger(x = X, y = y, probability = ctx$problem_type != "regression", num.threads = 4)
  },
  predict = function(model, X, ctx) predict(model, data = X, num.threads = 4)$predictions,
  preprocess = ta_prep_basic   # median/mode imputation, aligned factor levels
)

# 2. Evaluate on TabArena-Lite (the first split of every dataset) ...
res <- ta_evaluate(rf, splits = "lite")

# 3. ... and rank it against the published results of the leaderboard methods.
lb <- ta_leaderboard(res)          # bundled snapshot of the paper results
lb                                 # your method is marked with *
lb[lb$is_new, c("method", "elo", "elo_plus", "elo_minus", "score", "rank", "winrate")]

# One call for both steps:
out <- ta_benchmark(rf, splits = "lite")
out$summary
```

`ta_evaluate()` downloads each task's data (parquet) and the official
cross-validation split indices from OpenML on first use and caches them (see
`ta_cache_dir()`). Pass `results_dir = "somewhere"` to checkpoint every
finished split so a long run can be resumed.

### The learner contract

`train(X, y, ctx)` gets a data.frame of features (numeric and factor columns,
possibly with `NA`), the target (`factor` for classification, numeric for
regression) and a context list `ctx` (`dataset`, `split`, `problem_type`,
`metric`, `levels`, `positive_class`, ...). `predict(model, X, ctx)` must return

| problem type | return value |
| --- | --- |
| `regression` | numeric vector |
| `binary` | probability of `ctx$positive_class` (the second level), or a class-probability matrix |
| `multiclass` | class-probability matrix, columns named after `ctx$levels` |

Predictions are scored with the official metric: `1 - ROC AUC` (binary), log
loss (multiclass), RMSE (regression).

Models that want a formula interface just bind the target themselves:

```r
lin <- ta_learner(
  name = "glm / lm / multinom (R)",
  train = function(X, y, ctx) {
    d <- cbind(X, .y = y)
    switch(ctx$problem_type,
      regression = lm(.y ~ ., d),
      binary     = glm(.y ~ ., d, family = binomial()),
      multiclass = nnet::multinom(.y ~ ., d, trace = FALSE, MaxNWts = 20000))
  },
  predict = function(model, X, ctx) {
    switch(ctx$problem_type,
      regression = predict(model, X),
      binary     = predict(model, X, type = "response"),
      multiclass = predict(model, X, type = "probs"))
  },
  preprocess = function(X_train, X_test, ctx) ta_prep_basic(X_train, X_test, one_hot = TRUE)
)
ta_benchmark(lin, splits = "lite")$summary
```

Running that linear-model learner on TabArena-Lite took 30 minutes on a
laptop (including the OpenML downloads) and printed:

```
Fitting Elo (46 methods, 51 datasets, 51 splits, 100 bootstrap rounds) ...
glm / lm / multinom (R): Elo 695 (+117/-156), position 44 of 46 on 51 datasets / 51 splits.
TabArena leaderboard: 46 methods on 51 datasets / 51 splits  (* = your method)
 #  method                        elo  95% CI    score rank  harm. rank improv. % winrate ...
  1   AutoGluon 1.3 (4h)          1536 +69/-52   0.636  7.59  3.16       6.08     0.854
  2   REALMLP (tuned + ensemble)  1470 +69/-49   0.554  9.87  4.95       7.99     0.803
  3   GBM (tuned + ensemble)      1421 +56/-38   0.437 11.83  7.83       9.90     0.759
 ...
 37   RF (default)                1000 +50/-60   0.027 32.15 27.20      22.34     0.308
 ...
 44 * glm / lm / multinom (R)      695 +117/-156 0.024 41.20 29.26      45.66     0.107
 45   KNN (tuned)                  645 +107/-144 0.013 42.12 33.77      47.43     0.086
 46   KNN (default)                475 +123/-169 0.000 44.35 43.91      54.65     0.037
```

## What is computed, and how

The evaluation protocol is TabArena-v0.1: 51 curated OpenML tasks, 3-fold
outer cross-validation with 10 repeats for datasets under 2500 rows and 3
repeats otherwise (816 splits in total), the same split indices as the
official runs, and the official metric per task. `ta_tasks()` lists the
suite, `ta_task_grid()` the 816 (dataset, split) pairs; `splits = "lite"`
is TabArena-Lite (split 0 only).

`ta_leaderboard()` mirrors `bencheval`, the evaluation package of the
TabArena repository:

* **Elo**: every pair of methods "battles" on every (dataset, split) unit;
  lower error wins, equal error is half a win each; every dataset has total
  weight 1 whatever its number of splits; the Bradley-Terry model is fitted
  by maximum likelihood (with the reference implementation's tiny ridge so a
  dominated field stays finite); ratings are `400 * log10(strength) + 1000`,
  shifted so that `RF (default)` (a default random forest) is exactly 1000;
  the 95% interval comes from a dataset-level bootstrap (100 rounds).
* **Score** = 1 - normalized error, where the error of each method is averaged
  over the splits of a dataset and scaled between the best method (0) and the
  median method (1) on that dataset, clipped to [0, 1].
* **Rank**, **harmonic rank** (1 / mean reciprocal rank), **improvability**
  (`1 - best error / own error`) and **win rate** are computed per split, then
  averaged within each dataset, then across datasets.
* **Train / predict time per 1K rows**: median across datasets of the
  per-dataset mean of `time * 1000 / rows`.
* **Imputation**: with `imputation = FALSE` (the default, the website's
  "no imputation" board) a method missing any task of the field is dropped;
  with `imputation = TRUE` its missing tasks get the random forest's result and
  the share of imputed tasks is reported.

Two things to keep in mind when reading your Elo:

* The field matters. By default the reference methods are compared on exactly
  the (dataset, split) pairs your results cover (`splits = "evaluated"`), so a
  TabArena-Lite run is ranked on TabArena-Lite. Elo on a subset is not the
  official full-benchmark Elo; run `splits = "all"` for that.
* The official leaderboard methods were tuned and ensembled under TabArena's
  8-fold bagging protocol on specific hardware. A plain `train()` /
  `predict()` pair is closest to a "system" entrant. Timings are measured on
  your machine and are not comparable to the published ones.

## Reference results

| call | field |
| --- | --- |
| `ta_results("paper")` (default) | bundled snapshot of the paper leaderboard: 45 methods (default / tuned / tuned + ensembled variants of 16 models plus AutoGluon 1.3) on all 816 splits; works offline |
| `ta_results("live")` | the current per-method result files behind the online leaderboard (about 50 methods, 96 variants), downloaded from TabArena's public storage and cached; `use_display_names = TRUE` names them as the website does |

The live results are located through a method registry parsed from the
`autogluon/tabarena` sources. A snapshot is bundled; `ta_method_registry(refresh
= TRUE)` rebuilds it from GitHub so newly added methods are picked up without a
package update.

```r
live <- ta_results("live")
lb <- ta_leaderboard(res, reference = live, include_systems = FALSE)  # models-only pool, like the website
```

## Main functions

| function | purpose |
| --- | --- |
| `ta_tasks()`, `ta_task_grid()` | the task suite and its (dataset, split) grid, with named subsets (`"tiny"`, `"tabpfn"`, `"regression"`, ...) |
| `ta_load_task()`, `ta_split()` | download a task from OpenML, get the train/test data of one split |
| `ta_learner()`, `ta_prep_basic()` | wrap a train/predict pair; simple preprocessing |
| `ta_evaluate()`, `ta_evaluate_split()`, `ta_collect_results()` | run a learner, with checkpointing |
| `ta_metric_error()` | the official metrics |
| `ta_results()`, `ta_results_live()`, `ta_method_registry()`, `ta_registry_refresh()` | published results |
| `ta_leaderboard()`, `ta_elo()`, `ta_benchmark()` | leaderboard columns, Elo alone, or everything in one call |
| `ta_cache_dir()`, `ta_cache_clear()` | the download cache |

## Reference

Erickson, N., Purucker, L., Tschalzev, A., Holzmüller, D., Desai, P. S.,
Salinas, D., & Hutter, F. (2025). *TabArena: A Living Benchmark for Machine
Learning on Tabular Data.* NeurIPS 2025. https://arxiv.org/abs/2506.16791
