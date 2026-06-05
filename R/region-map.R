#' Map Total Measles Cases by Country for a WHO Region
#'
#' Creates a map of total measles cases by country for a chosen WHO region,
#' shaded by case count and summed across all available years in the data.
#'
#' @param data A data frame containing yearly measles case data, such as the
#'   `cases_year` tibble returned by [load_data()].
#' @param selected_region A character string specifying the WHO region code.
#'   One of "AFR", "AMR", "EMR", "EUR", "SEAR", "WPR".
#'
#' @return A ggplot object showing a map of total measles cases by country,
#'   shaded by case count.
#' @export
#'
#' @importFrom dplyr filter group_by summarise inner_join
#' @importFrom rnaturalearth ne_countries
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_gradient labs theme_void
#'
#' @examples
#' data <- load_data()
#' region_map(data$cases_year, "AMR")

region_map <- function(data, selected_region) {
  valid_regions <- c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR")

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!selected_region %in% valid_regions) {
    stop("`selected_region` must be one of: ",
         paste(valid_regions, collapse = ", "),
         call. = FALSE)
  }

  region_codes <- c(AFR = "AFRO", AMR = "AMRO", EMR = "EMRO",
                    EUR = "EURO", SEAR = "SEARO", WPR = "WPRO")
  region_names <- c(AFR = "Africa", AMR = "Americas",
                    EMR = "Eastern Mediterranean", EUR = "Europe",
                    SEAR = "South-East Asia", WPR = "Western Pacific")

  region_data <- data |>
    filter(region == region_codes[[selected_region]]) |>
    group_by(country, iso3) |>
    summarise(measles = sum(measles_total, na.rm = TRUE), .groups = "drop")

  world <- ne_countries(scale = "medium", returnclass = "sf")

  map_data <- world |>
    inner_join(region_data, by = c("iso_a3" = "iso3"))

  ggplot(map_data) +
    geom_sf(aes(fill = measles), color = "black", linewidth = 0.2) +
    scale_fill_gradient(low = "white", high = "darkred", name = "Total Cases") +
    labs(title = paste("Total Measles Cases by Country:",
                       region_names[[selected_region]])) +
    theme_void()
}
