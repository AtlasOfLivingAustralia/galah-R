# Object classes for `filter()` queries

In galah, there are several ways to provide filter information. To
ensure these are handled and printed correctly, they are assigned
classes

## Usage

``` r
as_data_filter(x)

as_predicates_filter(x)

as_metadata_filter(x)

as_files_filter(x)

# S3 method for class 'data_filter'
print(x, ...)

# S3 method for class 'predicates_filter'
print(x, ...)

# S3 method for class 'metadata_filter'
print(x, ...)

# S3 method for class 'files_filter'
print(x, ...)
```

## Arguments

- x:

  a list, or object of supported class

- ...:

  Additional arguments, currently ignored
