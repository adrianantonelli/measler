#' Plot Average Monthly Measles Cases by Region
#'
#' Creates a line plot of average monthly measles cases for a chosen WHO
#' region, with an optional filter for specific months.
#'
#' @param data A data frame containing monthly measles case data, defaulted to cases_month tibble on monthly measles case data.
#' @param selected_region A character string specifying the WHO region code.
#'   It can be one of the following listed: "AFR", "AMR", "EMR", "EUR", "SEAR", "WPR".
#' @param selected_months A numeric vector that can take month numbers between 1-12.
#'   If no months are chosen specifically, and its NULL then all months are included.
#'
#' @return A ggplot object showing average monthly measles cases.
#' @export
#'
#' @importFrom dplyr mutate recode
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap labs theme_linedraw scale_color_brewer theme element_text
#'
#' @examples
#' cases_month <- load_data()$cases_month
#' monthly_plot("AMR")
#' monthly_plot("EUR", selected_months = c(1, 2, 3))
monthly_plot <- function(selected_region, selected_months = NULL, data = load_data()$cases_month) {
  filter_region(data, selected_region, selected_months) |>
    mutate(month = factor(month.abb[month], levels = month.abb),
           region = recode(region,
          "AFR" = "Africa",
          "AMR" = "Americas",
          "EMR" = "Eastern Mediterranean",
          "EUR" = "Europe",
          "SEAR" = "South-East Asia",
          "WPR" = "Western Pacific")) |>
    ggplot(aes(x = month, y = avg_measles,
               group = region,color = region)) +
    geom_line() +
    facet_wrap(~region, scales = "free_y") +
    labs(x = "Month",
         title = "Measles Cases by Month Across the World",
         y = "",
         subtitle = "Number of Total Measles Cases (Clinical, EPI linked, and Lab Confirmed)") +
    theme_linedraw() +
    scale_color_brewer(palette = "Dark2") +
    theme(axis.text.x = element_text(size = 6),
          legend.position = "none")
}
