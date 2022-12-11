#' @export
csis_geocode <- function(address,
                         simple_geocode = FALSE,
                         headless = TRUE,
                         url_geocode = "https://geocode.csis.u-tokyo.ac.jp/geocode-cgi/geocode.cgi?action=start",
                         url_simple_geocode = "https://geocode.csis.u-tokyo.ac.jp/cgi-bin/simple_geocode.cgi",
                         geosys = "world",
                         constraint = NULL) {
  address <- tibble::tibble(address = address)

  if (simple_geocode) {
    pb <- progress::progress_bar$new(total = vec_size(address))

    address |>
      dplyr::rowwise() |>
      dplyr::mutate({
        out <- csis_simple_geocode(.data$address,
                                   url_simple_geocode = url_simple_geocode,
                                   geosys = geosys,
                                   constraint = constraint)
        Sys.sleep(1e-1)
        pb$tick()

        out
      }) |>
      dplyr::ungroup()
  } else {
    check_paths_allowed(url_geocode)

    file_address <- fs::file_temp(ext = "csv")
    readr::write_csv(address, file_address)

    download_dir <- fs::file_temp()
    driver <- driver_selenium(download_dir = download_dir,
                              headless = headless)

    driver$get(url_geocode)
    Sys.sleep(1)

    input_ncolumn <- driver$find_element(By$XPATH, "//input[@name='ncolumn']")
    input_ncolumn$send_keys("1")

    select_input_kanji_code <- Select(driver$find_element(By$XPATH, "//select[@name='input_kanji_code']"))
    select_input_kanji_code$select_by_value("utf8")

    input_file <- driver$find_element(By$XPATH, "//input[@name='file']")
    input_file$send_keys(stringr::str_replace_all(file_address, "/", "\\\\"))

    input_submit <- driver$find_element(By$XPATH, "//input[@type='submit']")
    input_submit$click()

    insistent_read_csv <- purrr::insistently(readr::read_csv)
    out <- insistent_read_csv(fs::path(download_dir, basename(file_address)),
                              col_types = readr::cols(address = "c",
                                                      LocName = "c",
                                                      fX = "n",
                                                      fY = "n",
                                                      iConf = "i",
                                                      iLvl = "i")) |>
      dplyr::rename(loc_name = "LocName",
                    X = "fX",
                    Y = "fY",
                    i_conf = "iConf",
                    i_lvl = "iLvl")
    driver$close()
    out
  }
}

csis_simple_geocode <- function(address, url_simple_geocode, geosys, constraint) {
  out <- httr::GET(url_simple_geocode,
                   query = purrr::compact(list(addr = address,
                                               charset = "UTF8",
                                               geosys = geosys,
                                               constraint = constraint))) |>
    httr::content()

  tibble::tibble(loc_name = out |>
                   xml2::xml_find_all("//address") |>
                   xml2::xml_text() |>
                   dplyr::first(),
                 X = out |>
                   xml2::xml_find_all("//longitude") |>
                   xml2::xml_double() |>
                   dplyr::first(),
                 Y = out |>
                   xml2::xml_find_all("//latitude") |>
                   xml2::xml_double() |>
                   dplyr::first(),
                 i_conf = out |>
                   xml2::xml_find_all("//iConf") |>
                   xml2::xml_integer() |>
                   dplyr::first(),
                 i_lvl = out |>
                   xml2::xml_find_all("//iLvl") |>
                   xml2::xml_integer() |>
                   dplyr::first())
}
