#' Test Whether Month Predicts Measles Cases by Region
#'
#' Runs separate one-way ANOVA F-tests to determine whether month is a
#' significant predictor of measles cases within one or more selected WHO
#' regions. The function returns a formatted gt table with one row per region.
#'
#' @param selected_regions A character vector of WHO region codes. Options are
#'   `"AFR"`, `"AMR"`, `"EMR"`, `"EUR"`, `"SEAR"`, and `"WPR"`.
#' @param selected_months A numeric vector of month numbers from 1 to 12.
#'   Defaults to all months.
#' @param data A data frame containing monthly measles data. Defaults to
#'   `load_data()$cases_month`.
#'
#' @return A gt table with ANOVA F-test results for each selected region.
#' @export
#'
#' @importFrom dplyr filter mutate tibble bind_rows
#' @importFrom gt gt fmt_number cols_label tab_header tab_style cell_text cell_fill cells_title cells_body
#' @importFrom stats lm anova
#'
#' @examples
#' month_f_test("AMR")
#' month_f_test(c("AMR", "AFR"))
#' month_f_test(c("AMR", "AFR"), selected_months = c(1, 2, 3))
month_f_test <- function(selected_regions,
                         selected_months = 1:12,
                         data = load_data()$cases_month) {

  valid_regions <- c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR")

  region_labels <- c("AFR" = "Africa",
                     "AMR" = "Americas",
                     "EMR" = "Eastern Mediterranean",
                     "EUR" = "Europe",
                     "SEAR" = "South-East Asia",
                     "WPR" = "Western Pacific")

  if (!all(selected_regions %in% valid_regions)) {
    stop("`selected_regions` must be from: ",
         paste(valid_regions, collapse = ", "),
         call. = FALSE)
  }

  if (!all(selected_months %in% 1:12)) {
    stop("`selected_months` must only contain month numbers from 1 to 12.",
         call. = FALSE)
  }

  get_region_test <- function(one_region) {

    filtered_data <- data |>
      filter(region == one_region,
             month %in% selected_months,
             measles_total >= 0) |>
      mutate(month = factor(month))

    if (nrow(filtered_data) == 0) {
      stop("No rows remain after filtering for region ",
           one_region,
           ". Check selected regions and months.",
           call. = FALSE)
    }

    month_model <- lm(measles_total ~ month, data = filtered_data)
    month_anova <- anova(month_model)

    f_stat <- month_anova$`F value`[1]
    p_val <- month_anova$`Pr(>F)`[1]
    df_num <- month_anova$Df[1]
    df_den <- month_anova$Df[2]

    conclusion <- ifelse(p_val < 0.05, "Significant", "Not Significant")

    tibble(Region = region_labels[[one_region]],
           Region_Code = one_region,
           DF_numerator = df_num,
           DF_denominator = df_den,
           F_statistic = f_stat,
           p_value = p_val,
           Conclusion = conclusion)
  }

  results <- bind_rows(
    lapply(selected_regions, get_region_test))

  results |>
    gt() |>
    fmt_number(columns = F_statistic, decimals = 2) |>
    fmt_number(columns = p_value, decimals = 4) |>
    cols_label(Region_Code = "Region Code",
               DF_numerator = "DF Numerator",
               DF_denominator = "DF Denominator",
               F_statistic = "F Statistic",
               p_value = "p-value") |>
    tab_header(title = "F-Test Results: Month as a Predictor of Measles Cases") |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_title(groups = "title")) |>
    tab_style(style = cell_fill(color = "lightgreen"),
              locations = cells_body(columns = Conclusion,
                                     rows = Conclusion == "Significant")) |>
    tab_style(style = cell_fill(color = "lightpink1"),
              locations = cells_body(
                columns = Conclusion,
                rows = Conclusion == "Not Significant"))
}
