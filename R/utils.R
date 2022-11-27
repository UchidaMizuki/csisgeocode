parse_year_ja <- function(x) {
  x <- x |>
    stringr::str_remove_all("\\s") |>
    stringi::stri_trans_nfkc()

  # year_ja
  pattern_era <- "(\u660e\u6cbb|\u5927\u6b63|\u662d\u548c|\u5e73\u6210|\u4ee4\u548c)" # Meiji, Taisho, Showa, Heisei, Reiwa
  era <- x |>
    stringr::str_extract(stringr::str_glue("^{pattern_era}"))
  year_ja <- x |>
    stringr::str_extract(stringr::str_glue("(?<={pattern_era})(\\d+|\u5143)(?=\u5e74?$)")) |> # Year
    stringr::str_replace("\u5143", "1") |> # First year (gan-nen)
    as.integer()

  # year
  year <- x |>
    stringr::str_extract("^\\d+(?=\u5e74?$)") |> # year
    as.integer()

  dplyr::case_when(is.na(era) ~ year,
                   era == "\u660e\u6cbb" ~ year_ja + 1868L - 1L, # Meiji
                   era == "\u5927\u6b63" ~ year_ja + 1912L - 1L, # Taisho
                   era == "\u662d\u548c" ~ year_ja + 1926L - 1L, # Showa
                   era == "\u5e73\u6210" ~ year_ja + 1989L - 1L, # Heisei
                   era == "\u4ee4\u548c" ~ year_ja + 2019L - 1L) # Reiwa
}

check_file <- purrr::insistently(
  function(file) {
    if (!fs::file_exists(file)) {
      abort()
    }
  },
  rate = purrr::rate_backoff(max_times = Inf)
)

check_paths_allowed <- function(url) {
  allowed <- purrr::quietly(robotstxt::paths_allowed)(url) |>
    purrr::chuck("result")

  if (!all(allowed)) {
    abort("A bot does not have permission to access the page.")
  }
}
