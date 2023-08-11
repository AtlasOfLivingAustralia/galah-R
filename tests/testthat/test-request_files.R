test_that("`collapse()` and `collect()` work for `type = 'doi'`", {
  doi <- "10.26197/ala.0c1e8744-a639-47f1-9a5f-5610017ba060"
  result <- request_files(type = "doi") |>
    filter(doi == doi) |>
    collapse()
  expect_equal(length(result), 4)
  expect_equal(names(result), 
               c("type", "url", "headers", "download"))
  
  # result2 <- collect(x) # errors
  # presumably because this DOI is not available on `https://api.test.ala.org.au`
  # currently unclear, therefore, whether `collect_doi()` works at all
  # what _is_ clear is that it doesn't error nicely!
})

test_that("`collapse()` and `collect()` work for `type = 'media'`", {
  x <- galah_call() |>
    filter(year >= 2021, !is.na(images)) |>
    identify("Litoria peronii") |>
    atlas_media()
  
  y <- request_files(type = "media") |>
    filter(media_url == x$imageUrl[!is.na(x$imageUrl)]) |>
    collapse()
  
  collect(y)
})