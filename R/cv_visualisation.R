
#'  Function to generate plot of cross-validation performance by region-year
#'
#' @details  Function to generate plot of cross-validation performance by region-year
#' @param cv_data cross validation data
#' @param plot_type scatter or dumbbell
#' @param dir_output_f direction output to save the dataset
#' @param save_plot save plot True or False
#' @param which_fold_f Name of the fold used
#' @param admin1_col Colnames of admin1
#' @export
f_cv_plot <- function(cv_data, admin1_col, plot_type = "scatter",
                      dir_output_f = dir_output,
                      save_plot = TRUE) {
  suppressWarnings({
    suppressMessages({
      result <- if ("scatter" %in% plot_type) {
        # If scatter plot ------------------------------------------------------

        cv_data$cv_by_admin1_year |>
          ggplot2::ggplot(ggplot2::aes(x = obs, y = pred)) +
          ggplot2::geom_point(ggplot2::aes(
            size = 2, alpha = 0.1,
            color = factor(year)
          )) +
          ggplot2::geom_abline(
            slope = 1, intercept = 0, linetype = "dashed",
            color = "black"
          ) +
          ggplot2::scale_color_viridis_d(option = "A", name = "Year") +
          ggplot2::theme_bw() +
          ggplot2::labs(
            title = "Actual vs. Predicted by Year",
            y = "Predicted deaths",
            x = "Actual deaths",
            fill = "Year"
          ) +
          ggplot2::guides(
            size = "none", alpha = "none",
            color = ggplot2::guide_legend(override.aes = list(size = 5))
          )
      } else if ("dumbbell"  %in% plot_type) {
        # If dumbbell plot -----------------------------------------------------

        resul <- cv_data$cv_by_admin1_year |>
          dplyr::mutate(bias_abs = pred - obs) |>
          ggplot2::ggplot(ggplot2::aes(
            x = 0, xend = bias_abs,
            y = reorder(!!admin1_col, bias_abs),
            group = !!admin1_col
          )) +
          ggalt::geom_dumbbell(
            colour = "red", size_xend = 3,
            colour_x = "#FAAB18", colour_xend = "#1380A1"
          ) +
          ggplot2::geom_vline(
            xintercept = 0, linewidth = .5,
            linetype = "dashed", color = "grey60"
          ) +
          ggplot2::facet_wrap(~year, scale = "free_x") +
          ggplot2::theme_minimal() +
          ggplot2::labs(
            x = "Difference (predicted - observed deaths)",
            y = "Region"
          ) +
          ggplot2::theme(
            axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
            axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
            plot.background = ggplot2::element_rect(fill = "white"),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
          )
      }

      # Save plot --------------------------------------------------------------

      if (!is.null(result) && save_plot) {
        class_f <- cv_data$class
        outcome <- cv_data$response
        file_name <- paste(dir_output_f, "/som_out_cv_perf_",
                           plot_type, "_", class_f, "_", outcome, ".png", sep = "")
        ggplot2::ggsave(file_name, plot = result,
                        dpi = "print", units = "cm", height = 21, width = 24)
      }
    })
  })
}
