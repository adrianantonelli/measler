# Internal helper utilities -------------------------------------------------
#' Filter and summarize monthly measles cases by region
#'
#' Internal helper that filters data by region and optionally by month,
#' then summarizes average measles cases.
#'
#' @param data A data frame containing monthly measles case data.
#' @param selected_region A character string specifying the WHO region code.
#' The acceptable region codes include:  One of "AFR", "AMR", "EMR", "EUR", "SEAR", "WPR".
#' @param selected_months Optional numeric vector of month numbers from 1-12.
#' If no months are chosen then all months are included.
#'
#' @return A summarized tibble with avg_measles per month and region.
#' @export

#' @importFrom dplyr filter group_by summarise
filter_region <- function(data, selected_region, selected_months = NULL) {
  valid_regions <- c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR")

  if (!selected_region %in% valid_regions) {
    stop("`selected_region` must be one of: ",
         paste(valid_regions, collapse = ", "),
         call. = FALSE)
  }

  if (!is.null(selected_months) && !all(selected_months %in% 1:12)) {
    stop("`selected_months` must be a numeric vector with values between 1 and 12.",
         call. = FALSE)
  }

  filtered_data <- data |>
    filter(region == selected_region,
      measles_total >= 0)

  if (!is.null(selected_months)) {
    filtered_data <- filtered_data |>
      filter(month %in% selected_months)
  }

  filtered_data |>
    group_by(month, region) |>
    summarise(
      avg_measles = mean(measles_total, na.rm = TRUE),
      .groups = "drop"
    )
}
