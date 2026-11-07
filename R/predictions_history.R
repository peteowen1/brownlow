# Dated history of published predictions ------------------------------------------
#
# The season predictions are rebuilt and overwritten, so once a version is gone
# nothing can be scored against what the model said at the time. Every publish
# therefore lands as a NEW parquet in the `predictions-history` release and no
# existing file is ever rewritten (a correction is a new file with a new time).

#' Publish One Dated Version of the Predictions
#'
#' Writes `predictions_<YYYYMMDDTHHMMSSZ>.parquet` (UTC publish time, to the
#' second) and uploads it to the `predictions-history` release. Refuses to
#' overwrite: it errors if a file of that name exists locally or on the release.
#'
#' @param predictions Data frame with at least `event`, `market`, `selection`
#'   (the model's own words for the outcome) and `probability` (in [0, 1]).
#'   Extra columns are kept. `verse` and `model_version` are filled in if absent.
#' @param verse Character. Value of the `verse` column when absent (default "brownlow").
#' @param model_version Character. Value of `model_version` when absent
#'   (default: the installed package version).
#' @param dir Directory the parquet is written to.
#' @param upload Logical. `FALSE` writes the file only (dry run).
#' @param repo,tag GitHub repo and release tag to upload to.
#'
#' @details Nothing in this repo calls this function yet: there is no live
#'   predictions publish step to hook it into (the predictions are not
#'   published anywhere automatically, and the 2027 season has not started).
#'   When one exists, call this right after the live upload succeeds, in its
#'   own `tryCatch()` so a history failure cannot break the live publish.
#'   Needs the suggested packages `arrow` and, when `upload = TRUE`, `piggyback`.
#'
#' @return The path of the parquet written (invisibly).
#' @export
publish_predictions_history <- function(predictions,
                                        verse = "brownlow",
                                        model_version = as.character(utils::packageVersion("brownlow")),
                                        dir = "results/predictions_history",
                                        upload = TRUE,
                                        repo = "peteowen1/brownlow",
                                        tag = "predictions-history") {
  rlang::check_installed("arrow", reason = "to write the predictions parquet")
  if (upload) rlang::check_installed("piggyback", reason = "to upload to the release")
  required <- c("event", "market", "selection", "probability")
  missing_cols <- setdiff(required, names(predictions))
  if (length(missing_cols)) stop("predictions is missing column(s): ", paste(missing_cols, collapse = ", "))
  if (!nrow(predictions)) stop("predictions has no rows")
  if (anyNA(predictions$probability) || any(predictions$probability < 0 | predictions$probability > 1)) {
    stop("probability must be non-missing and within [0, 1]")
  }

  now <- Sys.time()
  predictions <- tibble::as_tibble(predictions)
  if (!"verse" %in% names(predictions)) predictions$verse <- verse
  if (!"model_version" %in% names(predictions)) predictions$model_version <- model_version
  predictions$published_utc <- format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  front <- c("verse", "event", "market", "selection", "probability", "published_utc", "model_version")
  predictions <- predictions[, c(front, setdiff(names(predictions), front))]

  file_name <- format(now, "predictions_%Y%m%dT%H%M%SZ.parquet", tz = "UTC")
  path <- file.path(dir, file_name)
  if (file.exists(path)) stop(path, " already exists; a published file is never rewritten")

  if (upload) {
    # A listing failure means the release contents are UNKNOWN, not empty:
    # say so and stop, rather than skipping the no-overwrite check.
    existing <- tryCatch(piggyback::pb_list(repo = repo, tag = tag)$file_name,
                         error = function(e) {
                           message("Could not list release '", tag, "' on ", repo, ": ", conditionMessage(e))
                           NULL
                         })
    if (is.null(existing)) {
      stop("Cannot confirm ", file_name, " is absent from release ", tag,
           " (listing failed); not publishing")
    }
    if (file_name %in% existing) stop(file_name, " already exists on release ", tag)
  }

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  arrow::write_parquet(predictions, path)

  if (upload) {
    if (!tag %in% piggyback::pb_releases(repo = repo, verbose = FALSE)$tag_name) {
      piggyback::pb_release_create(repo = repo, tag = tag, body = "Dated history of published predictions. Files are never overwritten.")
    }
    piggyback::pb_upload(path, repo = repo, tag = tag, overwrite = FALSE)
  }
  invisible(path)
}
