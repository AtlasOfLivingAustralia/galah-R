# Partial taxonomic match message theming [plain]

    Code
      local({
        cli::cli_div(theme = list(span.bold = list(`font-weight` = "bold"),
        span.yellow = list(color = "yellow")))
        c(cli::cli_text(
          "Matched {.bold 2 of 3} taxonomic search terms in selected atlas (Australia)."),
        `!` = cli::cli_text("{.yellow 1 unmatched search term:}"), cli::cli_text(
          format_error_bullets(c(
            "{.yellow \"https://id.biodiversity.org.au/node/apni/291047\"}"))))
      })
    Message <cliMessage>
      Matched 2 of 3 taxonomic search terms in selected atlas (Australia).
      1 unmatched search term:
      * "https://id.biodiversity.org.au/node/apni/291047"
    Output
      NULL

# Partial taxonomic match message theming [ansi]

    Code
      local({
        cli::cli_div(theme = list(span.bold = list(`font-weight` = "bold"),
        span.yellow = list(color = "yellow")))
        c(cli::cli_text(
          "Matched {.bold 2 of 3} taxonomic search terms in selected atlas (Australia)."),
        `!` = cli::cli_text("{.yellow 1 unmatched search term:}"), cli::cli_text(
          format_error_bullets(c(
            "{.yellow \"https://id.biodiversity.org.au/node/apni/291047\"}"))))
      })
    Message <cliMessage>
      Matched [1m2 of 3[22m taxonomic search terms in selected atlas (Australia).
      [33m1 unmatched search term:[39m
      [36m*[39m [33m"https://id.biodiversity.org.au/node/apni/291047"[39m
    Output
      NULL

# Partial taxonomic match message theming [unicode]

    Code
      local({
        cli::cli_div(theme = list(span.bold = list(`font-weight` = "bold"),
        span.yellow = list(color = "yellow")))
        c(cli::cli_text(
          "Matched {.bold 2 of 3} taxonomic search terms in selected atlas (Australia)."),
        `!` = cli::cli_text("{.yellow 1 unmatched search term:}"), cli::cli_text(
          format_error_bullets(c(
            "{.yellow \"https://id.biodiversity.org.au/node/apni/291047\"}"))))
      })
    Message <cliMessage>
      Matched 2 of 3 taxonomic search terms in selected atlas (Australia).
      1 unmatched search term:
      • "https://id.biodiversity.org.au/node/apni/291047"
    Output
      NULL

# Partial taxonomic match message theming [fancy]

    Code
      local({
        cli::cli_div(theme = list(span.bold = list(`font-weight` = "bold"),
        span.yellow = list(color = "yellow")))
        c(cli::cli_text(
          "Matched {.bold 2 of 3} taxonomic search terms in selected atlas (Australia)."),
        `!` = cli::cli_text("{.yellow 1 unmatched search term:}"), cli::cli_text(
          format_error_bullets(c(
            "{.yellow \"https://id.biodiversity.org.au/node/apni/291047\"}"))))
      })
    Message <cliMessage>
      Matched [1m2 of 3[22m taxonomic search terms in selected atlas (Australia).
      [33m1 unmatched search term:[39m
      [36m•[39m [33m"https://id.biodiversity.org.au/node/apni/291047"[39m
    Output
      NULL

