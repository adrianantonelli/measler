#' @importFrom readr read_csv
#' @importFrom janitor clean_names row_to_names
#' @importFrom dplyr mutate across rename slice
load_data <- function(){
  cases_month <- readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_month.csv"
  ) |>
    clean_names() |>
    mutate(
      across(year:discarded, as.numeric)
    )

  cases_year <- readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_year.csv"
  ) |>
    row_to_names(1) |>
    clean_names() |>
    rename(
      country = member_state,
      iso3 = iso_country_code,
      measles_total= number_of_measles_cases_by_confirmation_method,
      measles_lab_confirmed = na,
      measles_epi_linked = na_2,
      measles_clinical = na_3,
      rubella_total = number_of_rubella_cases_by_confirmation_method,
      rubella_lab_confirmed = na_4,
      rubella_epi_linked = na_5,
      rubella_clinical = na_6
    ) |>
    slice(-1)
}




