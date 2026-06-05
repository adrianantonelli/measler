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
#'
#' @export
load_data <- function(){
  cases_month <- readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_month.csv") |>
    clean_names() |>
    mutate(across(year:discarded, as.numeric))

  cases_year <- readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_year.csv") |>
    clean_names() |>
    rename(measles_suspected_total = total_suspected_measles_rubella_cases,
      measles_incidence_rate = measles_incidence_rate_per_1000000_total_population,
      rubella_incidence_rate = rubella_incidence_rate_per_1000000_total_population,
      discarded_rate = discarded_non_measles_rubella_cases_per_100000_total_population)

  list(cases_month = cases_month,
    cases_year = cases_year
  )
}




