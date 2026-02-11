# Count the observations in each group

[`count()`](https://dplyr.tidyverse.org/reference/count.html) lets you
quickly count the unique values of one or more variables. It is
evaluated lazily.
[`add_count()`](https://dplyr.tidyverse.org/reference/count.html) is an
equivalent that uses `mutate()` to add a new column with group-wise
counts.

## Usage

``` r
# S3 method for class 'data_request'
count(x, ..., wt, sort, name)

# S3 method for class 'data_request'
add_count(x, ..., wt = NULL, sort = FALSE, name = NULL)
```

## Arguments

- x:

  An object of class `data_request`, created using
  [`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md)

- ...:

  currently ignored

- wt:

  currently ignored

- sort:

  currently ignored

- name:

  currently ignored
