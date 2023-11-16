# NOTE: Tests are skipped on GH Actions using `skip_on_ci()` to avoid throttling 
#       API 
#       (these are probably not built for many fast queries if output is large)

test_that("show_all checks for valid type input", {
  expect_error(show_all("nothing"))
  expect_error(show_all(FiElDs))
})

test_that("show_all parses ... correctly", {
  skip_if_offline(); skip_on_ci()
  fields1 <- show_all(fields)
  fields2 <- show_all("fields")
  expect_equivalent(fields1, fields2)
})

test_that("all show_all() functions return correctly with all syntax", {
  skip_if_offline(); skip_on_ci()
  valid_types <- c("apis",
                   "assertions",
                   "atlases",
                   "collections",
                   "datasets",
                   "fields",
                   "licences",
                   "lists",
                   "profiles",
                   "providers",
                   "ranks",
                   "reasons")
  invisible(lapply(valid_types, function(a){
    syntax1 <- paste0("show_all_", a) |> do.call(args = list()) # e.g. show_all_fields()
    syntax2 <- paste0("show_all(", a, ")") |> parse(text = _) |> eval() # e.g. show_all(fields)
    syntax3 <- request_metadata(type = a) |> collect()
    limit_test <- paste0("show_all_", a) |> do.call(args = list(limit = 3))
    expect_s3_class(syntax1, c("tbl_df", "tbl", "data.frame"))
    expect_equal(attributes(syntax1)$call, a)
    expect_equal(attributes(syntax1)$region, "Australia")
    expect_equal(syntax1, syntax2)
    expect_equal(syntax1, syntax3)
    expect_equal(nrow(limit_test), 3)
  }))
})