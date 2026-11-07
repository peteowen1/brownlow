test_that("publish_predictions_history writes a dated parquet with the required columns", {
  d <- file.path(tempfile("hist"))
  p <- data.frame(event = "Brownlow Medal", market = "Winner",
                  selection = c("A", "B"), probability = c(0.3, 0.7))
  f1 <- publish_predictions_history(p, dir = d, upload = FALSE)
  expect_match(basename(f1), "^predictions_\\d{8}T\\d{6}Z\\.parquet$")
  out <- arrow::read_parquet(f1)
  expect_true(all(c("verse", "event", "market", "selection", "probability",
                    "published_utc", "model_version") %in% names(out)))
  expect_equal(nrow(out), 2)

  # a later publish is a new file; the first is untouched
  Sys.sleep(1.1)
  f2 <- publish_predictions_history(p, dir = d, upload = FALSE)
  expect_false(f1 == f2)
  expect_true(file.exists(f1))
  expect_length(list.files(d), 2)
})

test_that("publish_predictions_history rejects bad input", {
  d <- tempfile("hist")
  ok <- data.frame(event = "e", market = "m", selection = "s", probability = 0.5)
  expect_error(publish_predictions_history(ok[, -4], dir = d, upload = FALSE), "missing column")
  ok$probability <- 1.5
  expect_error(publish_predictions_history(ok, dir = d, upload = FALSE), "within")
  expect_false(dir.exists(d))
})

test_that("upload path uploads once, with overwrite = FALSE, and refuses existing files", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("piggyback")
  p <- data.frame(event = "e", market = "m", selection = "s", probability = 0.5)
  uploaded <- list()
  testthat::local_mocked_bindings(
    pb_list = function(...) data.frame(file_name = "predictions_19700101T000000Z.parquet"),
    pb_releases = function(...) data.frame(tag_name = "predictions-history"),
    pb_upload = function(file, ..., overwrite) { uploaded[[length(uploaded) + 1]] <<- list(file = file, overwrite = overwrite); invisible(TRUE) },
    .package = "piggyback"
  )
  f <- publish_predictions_history(p, dir = tempfile("hist"), upload = TRUE)
  expect_length(uploaded, 1)
  expect_equal(uploaded[[1]]$file, f)
  expect_false(uploaded[[1]]$overwrite)
})

test_that("a name already on the release stops the publish before upload", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("piggyback")
  p <- data.frame(event = "e", market = "m", selection = "s", probability = 0.5)
  testthat::local_mocked_bindings(
    pb_list = function(...) data.frame(file_name = format(Sys.time() + 0:2, "predictions_%Y%m%dT%H%M%SZ.parquet", tz = "UTC")),
    pb_upload = function(...) stop("should not upload"),
    .package = "piggyback"
  )
  expect_error(publish_predictions_history(p, dir = tempfile("hist"), upload = TRUE), "already exists on release")
})

test_that("a failed release listing is reported and stops the publish", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("piggyback")
  p <- data.frame(event = "e", market = "m", selection = "s", probability = 0.5)
  d <- tempfile("hist")
  testthat::local_mocked_bindings(
    pb_list = function(...) stop("rate limited"),
    pb_upload = function(...) stop("should not upload"),
    .package = "piggyback"
  )
  expect_message(
    expect_error(publish_predictions_history(p, dir = d, upload = TRUE), "listing failed"),
    "rate limited")
  expect_false(dir.exists(d))
})
