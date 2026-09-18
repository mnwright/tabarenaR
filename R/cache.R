#' Location of the tabarenaR download cache
#'
#' Data, splits and reference results are cached here. The location is taken
#' from `getOption("tabarenaR.cache_dir")`, then from the environment variable
#' `TABARENA_R_CACHE`, then defaults to
#' `tools::R_user_dir("tabarenaR", "cache")`.
#'
#' @return The cache directory (created if needed).
#' @export
ta_cache_dir <- function() {
  dir <- getOption("tabarenaR.cache_dir", "")
  if (!nzchar(dir)) dir <- Sys.getenv("TABARENA_R_CACHE", unset = "")
  if (!nzchar(dir)) dir <- tools::R_user_dir("tabarenaR", which = "cache")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

#' Remove cached files
#'
#' @param what Which part of the cache to clear: `"all"`, `"openml"` (task
#'   data and splits), `"results"` (downloaded reference results) or
#'   `"registry"`.
#' @return Invisibly, the cache directory.
#' @export
ta_cache_clear <- function(what = c("all", "openml", "results", "registry")) {
  what <- match.arg(what)
  root <- ta_cache_dir()
  target <- if (what == "all") root else file.path(root, what)
  if (dir.exists(target)) unlink(target, recursive = TRUE, force = TRUE)
  if (what == "all") dir.create(root, recursive = TRUE, showWarnings = FALSE)
  invisible(root)
}

ta_verbose <- function() isTRUE(getOption("tabarenaR.verbose", TRUE))

ta_msg <- function(...) {
  if (ta_verbose()) message(...)
  invisible(NULL)
}

# Download `url` to `destfile` (atomically, with retries). Returns TRUE on
# success and FALSE when the resource is missing (HTTP error) and
# `must_exist = FALSE`.
ta_download <- function(url, destfile, retries = 3L, must_exist = TRUE) {
  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(destfile, ".part")
  last_error <- NULL
  for (i in seq_len(retries)) {
    ok <- tryCatch(
      {
        status <- suppressWarnings(utils::download.file(url, tmp, mode = "wb", quiet = TRUE))
        status == 0 && file.exists(tmp) && file.info(tmp)$size > 0
      },
      error = function(e) {
        last_error <<- conditionMessage(e)
        FALSE
      }
    )
    if (ok) {
      file.rename(tmp, destfile)
      return(TRUE)
    }
    if (file.exists(tmp)) unlink(tmp)
    # A 404 is not going to get better with retries.
    if (!is.null(last_error) && grepl("404|cannot open URL", last_error)) break
    Sys.sleep(min(2^i, 10))
  }
  if (must_exist) {
    stop("Failed to download ", url, if (!is.null(last_error)) paste0(": ", last_error), call. = FALSE)
  }
  FALSE
}

ta_read_json_url <- function(url, cache_file) {
  if (!file.exists(cache_file)) ta_download(url, cache_file)
  jsonlite::fromJSON(cache_file, simplifyVector = TRUE)
}
