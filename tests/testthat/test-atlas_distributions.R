# test_that("show_all_distributions() works", {
#   skip_if_offline()
#   x <- show_all_distributions()
#   expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
#   expect_gte(nrow(x), 1000)
#   expect_true(all(colnames(x) %in%
#                c("id", "family", "genus", "species", "common_name", 
#                  "taxon_concept_id", "label", "area_km", "data_resource_uid")))
#   y <- show_all(distributions)
#   expect_equal(x, y)
# })
# 
# test_that("`request_data(type = 'distributions')` works with `identify()`", {
#   skip_if_offline()
#   x <- galah_call(type = "distributions") |>
#     identify("Foa fo") |>
#     collect()
#   expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
#   expect_gte(nrow(x), 1)
#   expect_true(all(colnames(x) %in%
#                     c("id", "family", "genus", "species", "common_name", 
#                       "taxon_concept_id", "label", "area_km", "data_resource_uid",
#                       "geometry")))
#   # ditto with atlas_distributions
#   y <- galah_call() |>
#     identify("Foa fo") |>
#     atlas_distributions()
#   expect_equal(x, y)
# })
# 
# test_that("`request_data(type = 'distributions')` works with `filter()`", {
#   skip_if_offline()
#   x <- galah_call(type = "distributions") |>
#     filter(id == 25239) |>
#     collect()
#   expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
#   expect_gte(nrow(x), 1)
#   expect_true(all(colnames(x) %in%
#                     c("id", "family", "genus", "species", "common_name", 
#                       "taxon_concept_id", "label", "area_km", "data_resource_uid",
#                       "geometry")))
#   # ditto with atlas_distributions
#   y <- galah_call() |>
#     filter(id == 25239) |>
#     atlas_distributions()
#   expect_equal(x, y)
# })
# 
# test_that("request_data(type = 'distributions') works without filters", {
#   skip_if_offline()
#   x <- galah_call(type = "distributions") |>
#     collapse()
#   expect_s3_class(x, "query")
#   expect_gte(nrow(x$url), 1000)
#   n_rows <- 50
#   x$url <- x$url[seq_len(n_rows), ]  
#   y <- collect(x)
#   expect_equal(nrow(y), n_rows)
# })
# 
# test_that("atlas_distributions() fails when both identify and filter are supplied", {
#   skip_if_offline()
#   expect_error({galah_call() |>
#     identify("Foa fo") |>
#     filter(id == 25239) |>
#     atlas_distributions()})
# })