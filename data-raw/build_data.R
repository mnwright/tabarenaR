# Builds the files shipped in inst/extdata:
#   tabarena_v0.1_tasks.csv     task suite (from the curated metadata CSV of autogluon/tabarena)
#   tabarena_paper_results.rds  paper results snapshot (TabArena/benchmark_results on Hugging Face)
#   tabarena_methods.csv        method registry parsed from the tabarena GitHub sources
#
# Usage:  Rscript data-raw/build_data.R <source_dir>
# <source_dir> must contain curated_metadata.csv, df_results.csv and an info/ folder
# with the Python declaration files (or leave it empty to download everything).

args <- commandArgs(trailingOnly = TRUE)
src <- if (length(args)) args[1] else tempdir()
pkg <- getwd()  # run from the package root
if (!dir.exists(file.path(pkg, "R"))) stop("Run this script from the package root.")
for (f in list.files(file.path(pkg, "R"), full.names = TRUE)) source(f)
extdata <- file.path(pkg, "inst", "extdata")
dir.create(extdata, recursive = TRUE, showWarnings = FALSE)

gh <- "https://raw.githubusercontent.com/autogluon/tabarena/main/"

# ---- 1. task suite ---------------------------------------------------------
curated <- file.path(src, "curated_metadata.csv")
if (!file.exists(curated)) {
  ta_download(paste0(gh, "packages/tabarena/src/tabarena/benchmark/task/metadata/data/curated_tabarena_dataset_metadata.csv"), curated)
}
cm <- read.csv(curated, stringsAsFactors = FALSE)
metric_map <- c(binary = "roc_auc", multiclass = "log_loss", regression = "rmse")
tasks <- data.frame(
  dataset = cm$dataset_name,
  task_id = as.integer(cm$task_id),
  dataset_id = as.integer(cm$dataset_id),
  target = cm$target_feature,
  problem_type = cm$problem_type,
  metric = unname(metric_map[cm$problem_type]),
  n_folds = as.integer(cm$num_folds),
  n_repeats = as.integer(cm$tabarena_num_repeats),
  n_instances = as.integer(cm$num_instances),
  n_features = as.integer(cm$num_features) - 1L,   # curated count includes the target
  n_classes = ifelse(cm$problem_type == "regression", NA_integer_, as.integer(cm$num_classes)),
  pct_categorical = cm$percentage_cat_features,
  can_run_tabpfnv2 = cm$can_run_tabpfnv2 %in% c("True", "TRUE", TRUE),
  can_run_tabicl = cm$can_run_tabicl %in% c("True", "TRUE", TRUE),
  domain = cm$domain,
  source = cm$data_source,
  year = cm$year,
  licence = cm$licence,
  url = cm$original_data_url,
  stringsAsFactors = FALSE
)
tasks <- tasks[order(tasks$dataset), ]
stopifnot(nrow(tasks) == 51, !anyNA(tasks$task_id))
write.csv(tasks, file.path(extdata, "tabarena_v0.1_tasks.csv"), row.names = FALSE)
cat("tasks:", nrow(tasks), "\n")

# ---- 2. paper results ------------------------------------------------------
res_csv <- file.path(src, "df_results.csv")
if (!file.exists(res_csv)) {
  ta_download("https://huggingface.co/datasets/TabArena/benchmark_results/resolve/main/df_results.csv", res_csv)
}
res <- read.csv(res_csv, stringsAsFactors = FALSE)
res <- res[, c("dataset", "fold", "method", "metric_error", "time_train_s", "time_infer_s",
               "metric_error_val", "problem_type", "metric")]
# The paper leaderboard also lists AutoGluon 1.3 (best quality, 4h), whose rows
# are not part of the Hugging Face file; they are taken from TabArena's public
# store so the bundled snapshot matches the paper's 45-method field.
ag <- ta_results_method("AutoGluon_v130", "tabarena-2025-06-12", method_type = "baseline", cache_type = "s3")
ag$method <- "AutoGluon 1.3 (4h)"
res <- rbind(res, ag[, names(res)])
res$fold <- as.integer(res$fold)
stopifnot(!anyDuplicated(res[, c("dataset", "fold", "method")]))
stopifnot(all(res$dataset %in% tasks$dataset))
res <- res[order(res$method, res$dataset, res$fold), ]
rownames(res) <- NULL
saveRDS(res, file.path(extdata, "tabarena_paper_results.rds"), compress = "xz")
cat("paper results:", nrow(res), "rows,", length(unique(res$method)), "methods\n")

# ---- 3. method registry ----------------------------------------------------
info_dir <- file.path(src, "info")
reg <- if (dir.exists(info_dir) && length(list.files(info_dir, pattern = "\\.py$"))) {
  ta_registry_refresh(save = FALSE, src_dir = info_dir)
} else {
  ta_registry_refresh(save = FALSE)
}
write.csv(reg, file.path(extdata, "tabarena_methods.csv"), row.names = FALSE)
cat("registry:", nrow(reg), "declarations,", sum(reg$current), "current\n")
print(reg[reg$current, c("method", "suite", "method_type", "method_class", "cache_type")])
