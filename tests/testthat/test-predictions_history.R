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
