#' Summarizing Yearly Measles Cases for a Country
#'
#' Filters the yearly measles data to a single country and summarizes total
#' cases, average incidence rate, and discarded cases by year.
#'
#' @param data A data frame containing yearly measles case data, such as the
#' `cases_year` tibble returned by [load_data()].
#' @param selected_country A character string specifying the country name.
#'
#' @return A tibble with one row per year, containing `total_cases`,
#'   `avg_incidence_per_1M`, and `discarded_cases`.
#' @export
#'
#' @importFrom dplyr filter group_by summarise mutate arrange
#'
#' @examples
#' data <- load_data()
#' country_table(data$cases_year, "Brazil")
country_table <- function(data, selected_country) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!is.character(selected_country) || length(selected_country) != 1) {
    stop("`selected_country` must be a single character string.",
         call. = FALSE)
  }

  if (!selected_country %in% data$country) {
    stop("`selected_country` not found in the data's `country` column.",
         call. = FALSE)
  }

  data |>
    filter(country == selected_country) |>
    group_by(year) |>
    summarise(total_cases = sum(measles_total, na.rm = TRUE),
      avg_incidence_per_1M = mean(measles_incidence_rate, na.rm = TRUE),
      discarded_cases = sum(discarded_cases, na.rm = TRUE),
      .groups = "drop") |>
    mutate(
      avg_incidence_per_1M = round(avg_incidence_per_1M, 2)) |>
    arrange(year)
}
