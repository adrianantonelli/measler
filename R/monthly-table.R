#' Summarize Average Monthly Measles Cases by Region
#'
#' Returns a gt table of average monthly measles cases for chosen WHO
#' region, with an optional filter for specific months.
#'
#' @param data A data frame containing monthly measles case data.
#' @param selected_region A character string specifying the WHO region code.
#'   One of "AFR", "AMR", "EMR", "EUR", "SEAR", "WPR".
#' @param selected_months A numeric vector of month numbers from 1-12.
#'   If no months are chosen then all months are included.
#'
#' @return A gt table of average monthly measles cases for the selected region.
#' @export
#'
#' @importFrom dplyr mutate recode
#' @importFrom tidyr pivot_wider
#' @importFrom gt gt fmt_number cols_label tab_header tab_style tab_source_note cell_text cells_title
#'
#' @examples
#' monthly_table("AMR")
#' monthly_table("EUR", selected_months = c(1, 2, 3))
monthly_table <- function(selected_region, selected_months = NULL, data = load_data()$cases_month) {
  region_labels <- c(
    "AFR" = "Africa",
    "AMR" = "Americas",
    "EMR" = "Eastern Mediterranean",
    "EUR" = "Europe",
    "SEAR" = "South-East Asia",
    "WPR" = "Western Pacific")


  filter_region(data, selected_region, selected_months) |>
    mutate(
      month = factor(month.abb[month], levels = month.abb),
      region = recode(region,
                      "AFR" = "Africa",
                      "AMR" = "Americas",
                      "EMR" = "Eastern Mediterranean",
                      "EUR" = "Europe",
                      "SEAR" = "South-East Asia",
                      "WPR" = "Western Pacific"
      )
    ) |>
    pivot_wider(names_from = month,
                values_from = avg_measles) |>
    gt() |>
    fmt_number(
      columns = where(is.numeric),
      decimals = 2) |>
    cols_label(region = "Region") |>
    tab_header(
      title = paste("Average Monthly Measles Cases:",
                    region_labels[selected_region])) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_title(groups = "title")) |>
    tab_source_note(source_note = "Data source: WHO")
}
