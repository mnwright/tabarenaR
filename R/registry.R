#' Registry of methods with published TabArena results
#'
#' Every method benchmarked on TabArena is declared in the `tabarena` Python
#' package with a `method` name and a `suite` (the run it belongs to); the two
#' locate its results file in TabArena's public storage. The registry is a
#' table of these declarations. A snapshot is bundled with the package;
#' [ta_registry_refresh()] rebuilds it from the current source tree of the
#' `autogluon/tabarena` GitHub repository.
#'
#' @param refresh If `TRUE`, rebuild the registry from GitHub first (see
#'   [ta_registry_refresh()]); if `FALSE` use the refreshed copy in the cache
#'   when there is one, else the bundled snapshot.
#' @return A data.frame with columns `variable` (Python variable name),
#'   `method`, `suite`, `method_type`, `method_class`, `cache_type`,
#'   `display_name`, `verified`, `commercial_use`, `source_file` and
#'   `current` (`TRUE` for the methods in the current leaderboard
#'   collection).
#' @export
ta_method_registry <- function(refresh = FALSE) {
  cached <- file.path(ta_cache_dir(), "registry", "tabarena_methods.csv")
  if (refresh) return(ta_registry_refresh(save = TRUE))
  path <- if (file.exists(cached)) cached else system.file("extdata", "tabarena_methods.csv", package = "tabarenaR")
  reg <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("NA", ""))
  reg$current <- as.logical(reg$current)
  reg$verified <- as.logical(reg$verified)
  reg$commercial_use <- as.logical(reg$commercial_use)
  reg
}

#' Rebuild the method registry from the tabarena GitHub repository
#'
#' Downloads the method declaration files (`models/*/info.py`,
#' `systems/*/info.py`, `baselines/info.py`, the dated
#' `_tabarena_method_metadata_*.py` modules and `contexts/tabarena/methods.py`)
#' from `autogluon/tabarena` and extracts every `MethodMetadata` declaration
#' with a light-weight parser.
#'
#' @param ref Git ref (branch, tag or commit) to read.
#' @param save Store the result in the cache so [ta_method_registry()] uses
#'   it.
#' @param src_dir Optional directory that already holds the source files
#'   (used for offline rebuilds); when given nothing is downloaded.
#' @return The registry data.frame (see [ta_method_registry()]).
#' @export
ta_registry_refresh <- function(ref = "main", save = TRUE, src_dir = NULL) {
  if (is.null(src_dir)) {
    src_dir <- file.path(ta_cache_dir(), "registry", "src", ref)
    unlink(src_dir, recursive = TRUE)
    dir.create(src_dir, recursive = TRUE, showWarnings = FALSE)
    tree_file <- file.path(src_dir, "_tree.json")
    ta_download(paste0("https://api.github.com/repos/autogluon/tabarena/git/trees/", ref, "?recursive=1"), tree_file)
    tree <- jsonlite::fromJSON(tree_file)
    paths <- tree$tree$path
    pkg <- "packages/tabarena/src/tabarena/"
    want <- paths[grepl(paste0("^", pkg, "(models|systems)/[^/]+/info\\.py$"), paths) |
                    grepl(paste0("^", pkg, "baselines/info\\.py$"), paths) |
                    grepl(paste0("^", pkg, "contexts/tabarena/(_tabarena_method_metadata_.*|methods)\\.py$"), paths)]
    if (!length(want)) stop("No method declaration files found in the repository tree.", call. = FALSE)
    ta_msg("Downloading ", length(want), " method declaration files from autogluon/tabarena@", ref, " ...")
    for (p in want) {
      dest <- file.path(src_dir, ta_registry_local_name(p))
      ta_download(paste0("https://raw.githubusercontent.com/autogluon/tabarena/", ref, "/", p), dest)
    }
  }
  files <- list.files(src_dir, pattern = "\\.py$", full.names = TRUE)
  reg <- ta_registry_parse(files)
  if (save) {
    out <- file.path(ta_cache_dir(), "registry", "tabarena_methods.csv")
    dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(reg, out, row.names = FALSE)
  }
  reg
}

ta_registry_local_name <- function(path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  paste0(parts[length(parts) - 1], "__", parts[length(parts)])
}

# ---------------------------------------------------------------------------
# A small parser for the Python declaration files.

ta_py_strip_comments <- function(lines) {
  lines <- lines[!grepl("^\\s*#", lines)]
  # drop inline comments that contain no quote character (safe heuristic)
  sub("\\s+#[^\"']*$", "", lines)
}

# Extracts the text between the opening parenthesis at `pos` and its match.
ta_py_balanced <- function(txt, pos) {
  chars <- strsplit(substr(txt, pos, nchar(txt)), "")[[1]]
  depth <- 0L
  in_str <- ""
  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (nzchar(in_str)) {
      if (ch == in_str) in_str <- ""
      next
    }
    if (ch == "\"" || ch == "'") { in_str <- ch; next }
    if (ch %in% c("(", "[", "{")) depth <- depth + 1L
    if (ch %in% c(")", "]", "}")) {
      depth <- depth - 1L
      if (depth == 0L) return(paste(chars[2:(i - 1)], collapse = ""))
    }
  }
  NA_character_
}

ta_py_kwargs <- function(args) {
  out <- list()
  quoted <- character()
  m <- gregexpr("\\b([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*(\"([^\"]*)\"|'([^']*)'|([A-Za-z_][A-Za-z0-9_.]*)|(-?[0-9.]+))", args, perl = TRUE)
  hits <- regmatches(args, m)[[1]]
  for (h in hits) {
    key <- sub("\\s*=.*$", "", h)
    val <- sub("^[^=]*=\\s*", "", h)
    is_quoted <- grepl("^[\"']", val)
    val <- gsub("^[\"']|[\"']$", "", val)
    if (is.null(out[[key]])) {
      out[[key]] <- val
      if (is_quoted) quoted <- c(quoted, key)
    }
  }
  exp <- regmatches(args, gregexpr("\\*\\*([A-Za-z_][A-Za-z0-9_]*)", args, perl = TRUE))[[1]]
  out[["__expand__"]] <- sub("^\\*\\*", "", exp)
  out[["__quoted__"]] <- quoted
  out[["__has_cache_kwargs__"]] <- grepl("cache_kwargs\\s*=", args)
  out
}

# All `name = callee(...)` assignments of a file with their keyword arguments.
ta_py_assignments <- function(txt) {
  m <- gregexpr("(?m)^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*([A-Za-z_][A-Za-z0-9_.]*)\\(", txt, perl = TRUE)
  starts <- m[[1]]
  if (starts[1] == -1) return(list())
  lens <- attr(m[[1]], "match.length")
  out <- list()
  for (i in seq_along(starts)) {
    head <- substr(txt, starts[i], starts[i] + lens[i] - 1)
    var <- sub("\\s*=.*$", "", trimws(head))
    callee <- sub("\\($", "", sub("^[^=]*=\\s*", "", head))
    args <- ta_py_balanced(txt, starts[i] + lens[i] - 1)
    if (is.na(args)) next
    out[[var]] <- list(var = var, callee = callee, kwargs = ta_py_kwargs(args))
  }
  out
}

ta_registry_resolve_kwargs <- function(kw, assignments, depth = 0L) {
  exp <- kw[["__expand__"]]
  kw[["__expand__"]] <- NULL
  if (depth > 5L) return(kw)
  for (e in exp) {
    d <- assignments[[e]]
    if (is.null(d)) next
    inner <- ta_registry_resolve_kwargs(d$kwargs, assignments, depth + 1L)
    for (k in setdiff(names(inner), c("__quoted__", "__has_cache_kwargs__"))) {
      if (is.null(kw[[k]])) kw[[k]] <- inner[[k]]
    }
    kw[["__quoted__"]] <- union(kw[["__quoted__"]], inner[["__quoted__"]])
    kw[["__has_cache_kwargs__"]] <- isTRUE(kw[["__has_cache_kwargs__"]]) || isTRUE(inner[["__has_cache_kwargs__"]])
  }
  kw
}

ta_registry_parse_file <- function(file) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  txt <- paste(ta_py_strip_comments(lines), collapse = "\n")
  asg <- ta_py_assignments(txt)
  rows <- list()
  is_decl <- function(callee) {
    grepl("(^|\\.)method_metadata$", callee) ||
      grepl("^MethodMetadata(\\.(tabarena_legacy_s3|system|model|config|baseline))?$", callee)
  }
  for (a in asg) {
    if (!is_decl(a$callee)) next
    kw <- ta_registry_resolve_kwargs(a$kwargs, asg)
    method <- kw[["method"]]
    suite <- kw[["suite"]]
    if (is.null(method) || is.null(suite)) next
    if (!all(c("method", "suite") %in% kw[["__quoted__"]])) next
    is_system <- identical(kw[["method_class"]], "system") || grepl("\\.system$", a$callee)
    method_type <- kw[["method_type"]]
    if (is.null(method_type)) method_type <- if (is_system) "baseline" else "config"
    cache_type <- kw[["cache_type"]]
    if (is.null(cache_type)) {
      cache_type <- if (grepl("tabarena_legacy_s3$", a$callee)) "s3" else if (isTRUE(kw[["__has_cache_kwargs__"]])) "r2" else "local"
    }
    display <- kw[["name"]]
    if (is.null(display)) display <- kw[["display_name"]]
    if (is.null(display) && grepl("\\.method_metadata$", a$callee)) {
      # `<descriptor>.method_metadata(...)`: the display name lives on the descriptor
      desc <- asg[[sub("\\.method_metadata$", "", a$callee)]]
      if (!is.null(desc)) display <- desc$kwargs[["display_name"]]
    }
    rows[[length(rows) + 1L]] <- data.frame(
      variable = a$var, method = method, suite = suite, method_type = method_type,
      method_class = if (is_system) "system" else "model", cache_type = cache_type,
      display_name = if (is.null(display)) NA_character_ else display,
      verified = if (is.null(kw[["verified"]])) NA else identical(kw[["verified"]], "True"),
      commercial_use = if (is.null(kw[["commercial_use"]])) TRUE else !identical(kw[["commercial_use"]], "False"),
      source_file = basename(file), stringsAsFactors = FALSE
    )
  }
  # The 2025-06-12 factory loop: `methods = [...]` built with tabarena_legacy_s3.
  if (grepl("_2025_06_12", basename(file))) {
    lst <- regmatches(txt, regexpr("(?s)\\nmethods = \\[(.*?)\\]", txt, perl = TRUE))
    if (length(lst)) {
      names_in <- regmatches(lst, gregexpr("\"([^\"]+)\"", lst))[[1]]
      names_in <- gsub("\"", "", names_in)
      for (nm in names_in) {
        rows[[length(rows) + 1L]] <- data.frame(
          variable = paste0("factory:", nm), method = nm, suite = "tabarena-2025-06-12",
          method_type = "config", method_class = "model", cache_type = "s3",
          display_name = NA_character_, verified = NA, commercial_use = TRUE,
          source_file = basename(file), stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

# Names of the variables listed in the current leaderboard collection of
# methods.py, resolved through its import aliases.
ta_registry_collection <- function(methods_file) {
  lines <- readLines(methods_file, warn = FALSE, encoding = "UTF-8")
  txt <- paste(ta_py_strip_comments(lines), collapse = "\n")
  # import aliases: "orig as alias" -> alias -> orig
  alias <- list()
  clean <- ta_py_strip_comments(lines)
  i <- 1L
  while (i <= length(clean)) {
    ln <- clean[i]
    if (grepl("^\\s*from\\s+[A-Za-z0-9_.]+\\s+import\\s+", ln)) {
      body <- sub("^\\s*from\\s+[A-Za-z0-9_.]+\\s+import\\s+", "", ln)
      if (grepl("\\($", trimws(body))) {
        body <- ""
        i <- i + 1L
        while (i <= length(clean) && !grepl("^\\s*\\)", clean[i])) {
          body <- paste(body, clean[i])
          i <- i + 1L
        }
      }
      for (item in trimws(strsplit(body, ",")[[1]])) {
        if (!nzchar(item)) next
        parts <- strsplit(item, "\\s+as\\s+")[[1]]
        alias[[parts[length(parts)]]] <- parts[1]
      }
    }
    i <- i + 1L
  }
  block <- regmatches(txt, regexpr("tabarena_method_metadata_collection = MethodMetadataCollection\\(", txt))
  pos <- regexpr("tabarena_method_metadata_collection = MethodMetadataCollection\\(", txt)
  if (pos == -1) stop("Could not find the leaderboard collection in methods.py", call. = FALSE)
  args <- ta_py_balanced(txt, pos + attr(pos, "match.length") - 1)
  lst <- regmatches(args, regexpr("\\[(?s).*\\]", args, perl = TRUE))
  ids <- regmatches(lst, gregexpr("[A-Za-z_][A-Za-z0-9_]*", lst))[[1]]
  ids <- setdiff(ids, c("method_metadata_lst"))
  resolved <- vapply(ids, function(id) if (!is.null(alias[[id]])) alias[[id]] else id, character(1))
  # special declarations inside methods.py, e.g. TabPFNv2_GPU pulled from the 2025-06-12 factory
  special <- regmatches(txt, gregexpr("([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*next\\(m for m in methods_2025_06_12 if m\\.method == \"([^\"]+)\"\\)", txt, perl = TRUE))[[1]]
  special_map <- list()
  for (s in special) {
    var <- sub("\\s*=.*$", "", s)
    meth <- sub(".*m\\.method == \"([^\"]+)\".*", "\\1", s)
    special_map[[var]] <- meth
  }
  list(ids = unname(resolved), special = special_map)
}

ta_registry_parse <- function(files) {
  methods_file <- files[basename(files) %in% c("tabarena__methods.py", "methods.py")]
  decl_files <- setdiff(files, methods_file)
  reg <- do.call(rbind, lapply(decl_files, ta_registry_parse_file))
  reg <- reg[!duplicated(reg[, c("method", "suite")]), , drop = FALSE]
  reg$current <- FALSE
  if (length(methods_file)) {
    col <- ta_registry_collection(methods_file[1])
    reg$current <- reg$variable %in% col$ids
    for (var in names(col$special)) {
      hit <- which(reg$method == col$special[[var]] & reg$suite == "tabarena-2025-06-12")
      if (length(hit)) reg$current[hit[1]] <- TRUE
    }
    missing <- setdiff(col$ids, c(reg$variable, names(col$special)))
    if (length(missing)) {
      warning("Leaderboard collection entries not found among declarations: ", paste(missing, collapse = ", "), call. = FALSE)
    }
  }
  reg <- reg[order(!reg$current, reg$suite, reg$method), , drop = FALSE]
  rownames(reg) <- NULL
  reg
}
