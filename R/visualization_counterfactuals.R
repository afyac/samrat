#' Function to return clean excess counterfactuals table
#'
#' @details Function to return clean counterfactuals table
#' @param excess_res Excess Results
#' @export
f_clean_counterfactuals_table <- function(excess_res){
  excess_est_full <- excess_res |>
    dplyr::bind_rows(
      excess_res |> dplyr::summarise(
        dplyr::across(
          starts_with('toll_'), sum,
          na.rm = T
        )
      )  |>
        dplyr::mutate(district = "Total")
    ) |>
    dplyr::mutate(dplyr::across(starts_with('toll_'), round, -2)) |>
    dplyr::mutate(dplyr::across(starts_with('dr_'), round, 3)) |>
    dplyr::mutate(dplyr::across(where(is.numeric), format, big.mark = ",")) |>
    dplyr::summarise(
      # Estimates
      `Excess Toll (Overall)` = glue::glue("{toll_excess_period} ({toll_excess_period_low} to {toll_excess_period_up})"),
      `Excess Death Rate (Overall)` = glue::glue("{dr_excess_period} ({dr_excess_period_low} to {dr_excess_period_up})"),
      `Excess Toll (Under 5)` = glue::glue("{toll_excess_period_u5} ({toll_excess_period_u5_low} to {toll_excess_period_u5_up})"),
      `Excess Death Rate (Under 5)` = glue::glue("{dr_excess_period_u5} ({dr_excess_period_u5_low} to {dr_excess_period_u5_up})"),
      .by = "district"
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character), ~ stringr::str_replace_all(., " ", "")
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character), ~ stringr::str_replace_all(., "\\(", " (")
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character), ~ stringr::str_replace_all(., "to", " to ")
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::rename(district = district)
  return(excess_est_full)
}

#' Function to return clean actual counterfactuals table
#'
#' @details Function to return clean actual counterfactuals table
#' @param actual_res Actual Results
#' @export
f_clean_actual_table <- function(actual_res){
  actual_est_full <- actual_res |>
    dplyr::bind_rows(
      actual_res |> dplyr::summarise(
        dplyr::across(
          tidyselect::where(is.numeric), sum,
          na.rm = T
        )
      ) |>
        dplyr::mutate(district = "Total")
    ) |>
    dplyr::mutate(dplyr::across(starts_with('toll_'), round, -2)) |>
    dplyr::mutate(dplyr::across(starts_with('dr_'), round, 3)) |>
    dplyr::mutate(dplyr::across(where(is.numeric), format, big.mark = ",")) |>
    dplyr::summarise(
      # Estimates
      `Actual Toll (Overall)` = glue::glue("{toll_mean} ({toll_low} to {toll_up})"),
      `Actual Death Rate (Overall)` = glue::glue("{dr_mean} ({dr_low} to {dr_up})"),
      `Actual Toll (Under 5)` = glue::glue("{toll_mean_u5} ({toll_low_u5} to {toll_up_u5})"),
      `Actual Death Rate (Under 5)` = glue::glue("{dr_mean_u5} ({dr_low_u5} to {dr_up_u5})"),
      .by = "district"
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character), ~ stringr::str_replace_all(., " ", "")
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character), ~ stringr::str_replace_all(., "\\(", " (")
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character), ~ stringr::str_replace_all(., "to", " to ")
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::rename(district = district)
  return(actual_est_full)
}


#' Function to to plot country level curves with boostrapping
#'
#' @details Function to to plot country level curves with boostrapping
#' @param boostrapping_results Boostrapping results
#' @param name_output name of the output to save the results
#' @param path_output path of the folder to save the results
#' @param resp_var response variable
#' @param start_date_cf_a start date for counterfactual period 1
#' @param end_date_cf_a end date for counterfactual period 1
#' @param start_date_cf_b start date for counterfactual period 2
#' @param end_date_cf_b end date for counterfactual period 2
#' @param cf_b_label name of counterfactual period 2
#' @param cf_a_label name of counterfactual period 1
#' @export
f_plot_country_plot_with_boostrap <- function(boostrapping_results,
                                              name_output,
                                              path_output = '08_define_final_model/visualisation/output/',
                                              resp_var='n_died',
                                              start_date_cf_a = "2015-01-01",
                                              end_date_cf_a = "2016-06-01",
                                              start_date_cf_b = "2020-01-01",
                                              end_date_cf_b = "2021-06-01",
                                              cf_b_label = "2015 counterfactual",
                                              cf_a_label = "2020 counterfactual"){

  palette_cb <- c("#999999", "#E69F00", "#E0EEEE", "#009E73", "#F0E442",
                  "#104E9B", "#D55E00", "#CC79A7")

  if (resp_var %in% c("cdr", "n_died")) {resp_var <- "cdr"}
  if (resp_var %in% c("cdr_u5", "n_died_u5")) {resp_var <- "cdr_u5"}

  dr_lab <- dplyr::if_else(resp_var == "cdr",
                           "Estimated crude death rate (per 10,000 person-days) \n",
                           "Estimated under 5 years death rate (per 10,000 child-days) \n")
  annot <- dplyr::if_else(resp_var == "cdr", '', '_u5')

  if (resp_var == "cdr") {
    y_label <- "Estimated crude death rate (per 10,000 person-days) \n"
  } else if (resp_var == "cdr_u5") {
    y_label <- "Estimated under 5 years death rate (per 10,000 child-days) \n"
  } else {stop("Invalid type provided!")  }

  df_plot <- f_generate_final_results(boostrapping_results,
                                              agg_level = 'date',
                                              nb_boostrap = 1000,
                                              resp_var = resp_var)

  countfact_mean_a <- df_plot |>
    dplyr::filter(date > as.Date(start_date_cf_a) & date < as.Date(end_date_cf_a)) |>
    dplyr::summarise(dr = mean(get(paste('dr_mean', annot, sep='')))) |>
    dplyr::pull()

  countfact_mean_b <- df_plot |>
    dplyr::filter(date > as.Date(start_date_cf_b) & date < as.Date(end_date_cf_b)) |>
    dplyr::summarise(dr= mean(get(paste('dr_mean', annot, sep='')))) |>
    dplyr::pull()

  plot <- df_plot |>
    ggplot2::ggplot(
      ggplot2::aes(x = date, y = df_plot[,paste('dr_mean', annot, sep='')],
          ymin = df_plot[,paste('dr_low', annot, sep='')],
          ymax = df_plot[,paste('dr_up', annot, sep='')])) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=df_plot[,paste('dr_low', annot, sep='')],
                                      ymax=df_plot[,paste('dr_up', annot, sep='')]),
      alpha = 0.1, fill = "#0072B2") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=df_plot[,paste('dr_25', annot, sep='')],
                    ymax=df_plot[,paste('dr_75', annot, sep='')]), alpha=0.1,
                fill = "#0072B2")+
    ggplot2::geom_point(
      alpha = 0.3, size = 2, colour = "#D55E00") +
    ggplot2::geom_smooth(
      ggplot2::aes(ymin = NULL, ymax = NULL),
      se = FALSE, span = 0.15, linewidth = 1, colour = palette_cb[7]) +
    # Thick line four counterfactual period a
    ggplot2::geom_segment(
      ggplot2::aes(x = as.Date(start_date_cf_b), xend = as.Date(end_date_cf_b),
          y = countfact_mean_b,
          yend = countfact_mean_b),
      color = palette_cb[6], size = 0.9) +
    # Dotted line four counterfactual period a
    ggplot2::geom_segment(
      ggplot2::aes(x = as.Date(start_date_cf_b), xend = max(date),
          y = countfact_mean_b,
          yend = countfact_mean_b),
      color = palette_cb[6], linetype = "dotted", size = 0.9) +
    ggplot2::geom_text(
      ggplot2::aes(x = max(date), y = countfact_mean_b, label = cf_a_label),
      hjust = -0.075, vjust = 1.4, color = palette_cb[6], size=5) +
    # Thick line four counterfactual period b
    ggplot2::geom_segment(
      ggplot2::aes(x = as.Date(start_date_cf_a), xend = as.Date(end_date_cf_a),
          y = countfact_mean_a, yend = countfact_mean_a),
      alpha = 1, color = palette_cb[4], size = 0.9) +
    # Dotted line four counterfactual period b
    ggplot2::geom_segment(
      ggplot2::aes(x = as.Date(start_date_cf_a), xend = max(date),
          y = countfact_mean_a,
          yend = countfact_mean_a),
      color = palette_cb[4], linetype = "dotted", size = 0.9) +
    # Title for counterfactual period a
    ggplot2::geom_text(
      ggplot2::aes(x = max(date), y = countfact_mean_a,
          label = cf_b_label),
      hjust = -0.073, vjust = 0.4, color = palette_cb[4], size=5) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.margin = ggplot2::margin(0.5, 5, 0.5, 0.5, "cm"),
                   axis.text.x = ggplot2::element_text(size=20, angle = 45, vjust=1, hjust=1),
                   axis.text.y = ggplot2::element_text(size=20),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(size=20),
                   legend.text = ggplot2::element_text(size=20)) +
    ggplot2::scale_y_continuous(y_label, expand = c(0, 0), limits = c(min(df_plot[,paste('dr_low', annot, sep='')]), max(df_plot[,paste('dr_up', annot, sep='')]))) +
    ggplot2::scale_x_date("\n Year", date_breaks = "1 year",
                 date_labels = "%Y", expand = ggplot2::expansion(add = 12))


  return(plot)
}
