webdriver <- NULL
Service <- NULL
By <- NULL
Select <- NULL
# ActionChains <- NULL
ChromeDriverManager <- NULL

driver_selenium <- function(download_dir,
                            headless = TRUE) {
  options <- webdriver$ChromeOptions()
  options$headless <- headless
  # options$add_argument(stringr::str_glue("user-agent={user_agent()}"))

  prefs <- list(`download.default_directory` = fs::path_abs(download_dir) |>
                  stringr::str_replace_all("/", r"(\\)"))
  options$add_experimental_option("prefs", prefs)

  webdriver$Chrome(service = Service(ChromeDriverManager()$install()),
                   options = options)
}
