#' Load measles and rubella case data
#'
#' Downloads the WHO measles and rubella case data from the TidyTuesday
#' repository (2025-06-24) and returns cleaned monthly and yearly data sets.
#'
#' @return A named list with two tibbles: `cases_month` (monthly data) and
#'   `cases_year` (yearly data), both with cleaned column names.
#'
#' @importFrom readr read_csv
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate across rename
#' @importFrom arrow read_parquet
#'
#' @export
load_data <- function(){
  path <- system.file("extdata", "complete_data.parquet", package = "measler")


  cases_month <- read_parquet(path) |>
    clean_names() |>
    mutate(across(year:incidence, as.numeric))

  cases_year <- read_parquet(path) |>
    clean_names() |>
    rename(measles_suspected_total = yearly_total_suspected_measles_rubella_cases,
           measles_incidence_rate = yearly_measles_incidence_rate_per_1000000_total_population,
           rubella_incidence_rate = yearly_rubella_incidence_rate_per_1000000_total_population,
           discarded_rate = yearly_discarded_non_measles_rubella_cases_per_100000_total_population)

  list(cases_month = cases_month,
       cases_year = cases_year
  )
}




