## FIXME: collect_occurrences() is no longer the function to download data with a doi
## Update to new method
test_that("collect_occurrences downloads data from a DOI", {
  skip_on_cran()
    doi <- "10.26197/ala.0c1e8744-a639-47f1-9a5f-5610017ba060"
    occ <- collect_occurrences(doi = doi)
    expect_true(inherits(occ, c("tbl_df", "tbl", "data.frame")))
    expect_gt(nrow(occ), 0)
})

## FIXME: collect_occurrences() is no longer the function to do this with
## Update to new method
test_that("collect_occurrences checks DOI provided", {
  expect_error(collect_occurrences(doi = "random_doi"))
})

# Future: collect_occurrences provides a deprecated function message