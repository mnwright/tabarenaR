# Reproduces the public TabArena leaderboard from the live per-method results
# and compares every column with the CSV behind the website
# (entrants_open = models + open-source systems, no imputation, all splits, all
# datasets). Run from the package root after installing the package:
#   Rscript data-raw/validate_against_website.R
library(tabarenaR)

website_csv <- "https://huggingface.co/spaces/TabArena/leaderboard/resolve/main/data/entrants_open/imputation_no/splits_all/tasks_all/datasets_all/website_leaderboard.csv"
tmp <- tempfile(fileext = ".csv")
download.file(website_csv, tmp, mode = "wb", quiet = TRUE)
web <- read.csv(tmp, check.names = FALSE, encoding = "UTF-8")
names(web) <- sub(" \\[.*$", "", names(web))
web$Model <- sub("^\\[([^]]*)\\].*$", "\\1", web$Model)

live <- ta_results_live(verbose = FALSE)
lb <- ta_leaderboard(NULL, reference = live, splits = "all", imputation = FALSE,
                     include_systems = TRUE, bootstrap_rounds = 100, verbose = FALSE)

norm <- function(x) {
  x <- tolower(gsub("\\(tuned \\+ ensembled\\)", "(tuned + ensemble)", x))
  trimws(gsub("\\s+", " ", gsub("[^a-z0-9()+ ]", "", x)))
}
mine <- as.data.frame(lb)
mine$key <- norm(mine$method)
web$key <- norm(web$Model)
cmp <- merge(mine, web, by = "key", all = TRUE)
ok <- !is.na(cmp$elo) & !is.na(cmp$Elo)
cat(sprintf("methods: %d matched, %d only here, %d only on the website\n",
            sum(ok), sum(!is.na(cmp$elo) & is.na(cmp$Elo)), sum(is.na(cmp$elo) & !is.na(cmp$Elo))))
cmp <- cmp[ok, ]
report <- function(label, a, b, rounding) {
  cat(sprintf("%-28s max |diff| = %.4f   (website rounded to %s)\n", label, max(abs(a - b)), rounding))
}
report("Elo", cmp$elo, cmp$Elo, "integers")
report("normalized score", cmp$score, cmp$Score, "3 decimals")
report("mean rank", cmp$rank, cmp$Rank, "2 decimals")
report("harmonic rank", cmp$harmonic_rank, cmp$`Harmonic Rank`, "2 decimals")
report("improvability (%)", cmp$improvability_pct, cmp$`Improvability (%)`, "3 decimals")
report("median train time (s/1K)", cmp$median_time_train_s_per_1K, cmp$`Median Train Time (s/1K)`, "2 decimals")
report("median predict time (s/1K)", cmp$median_time_infer_s_per_1K, cmp$`Median Predict Time (s/1K)`, "3 decimals")
ci <- sprintf("+%.0f/-%.0f", cmp$elo_plus, cmp$elo_minus)
cat(sprintf("%-28s %d of %d identical strings (bootstrap draws differ by RNG)\n", "Elo 95% CI", sum(ci == cmp$`Elo 95% CI`), nrow(cmp)))
