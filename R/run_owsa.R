#' Run One-Way Sensitivity Analysis (OWSA)
#'
#' This function runs One-Way Sensitivity Analysis (OWSA) for a given model
#' function.
#'
#' @inheritParams plot_owsa
#'
#' @return A list containing the one-way sensitivity analysis (OWSA) results by
#' intervention.
#' @export
#'
#' @examples
#' \dontrun{
#' df_owsa_results <- run_owsa(
#'     model_func = NeuroblastomaPSM::run_psm,
#'     model_func_args = list(
#'       "models_fit" = NeuroblastomaPSM::parametric_models,
#'       "l_params" = c(
#'         time_horizon = 10,
#'         cycle_length = 1/12,
#'         disc_rate_costs = 0.035,
#'         disc_rate_qalys = 0.015,
#'         NeuroblastomaPSM::l_psm_parameters
#'       )
#'     ),
#'     v_dsa_params = names(NeuroblastomaPSM::l_dsa_parameters),
#'     l_dsa_params = NeuroblastomaPSM::l_dsa_parameters,
#'     discounted_output = FALSE,
#'     wtp = 30000
#' )
#' }
run_owsa <- function(
    model_func = NeuroblastomaPSM::run_psm,
    model_func_args = list(
      "models_fit" = NeuroblastomaPSM::parametric_models,
      "l_params" = c(
        time_horizon = 10,
        cycle_length = 1/12,
        disc_rate_costs = 0.035,
        disc_rate_qalys = 0.015,
        NeuroblastomaPSM::l_psm_parameters
      )
    ),
    v_dsa_params = names(NeuroblastomaPSM::l_dsa_parameters),
    l_dsa_params = NeuroblastomaPSM::l_dsa_parameters,
    discounted_output = FALSE,
    wtp = 30000) {

  # Prepare parameters data.frame:
  v_ci_bounds_names <- names(l_dsa_parameters[[1]])

  # Deterministic model run:
  v_deterministic_run <- do.call(
    model_func,
    model_func_args
  )

  # Run OWSA saving results by intervention:
  df_owsa_results <- lapply(
    X = v_ci_bounds_names,
    FUN = function(ci_bound_name) {
      lapply(
        X = v_dsa_params,
        FUN = function(owsa_parameter) {
          # Override values in PSM parameters list
          model_func_args$l_params[[owsa_parameter]] <-
            l_dsa_params[[owsa_parameter]][[ci_bound_name]]

          # Simulate OWSA parameter configuration
          v_run_results <- do.call(
            model_func,
            model_func_args
          )

          # Extract results:
          data.frame(
            "intervention" = c("Isotretinoin", "Dinutuximab β"),
            "parameter" = owsa_parameter,
            "ci_bound_label" = ci_bound_name,
            "ci_bound_value" = l_dsa_params[[owsa_parameter]][[ci_bound_name]],
            "effects" = if(!discounted_output) {
              c(v_run_results[["qalys_Isotretinoin"]],
                v_run_results[["qalys_Dinutuximab_β"]])
            } else {
              c(v_run_results[["Dqalys_Isotretinoin"]],
                v_run_results[["Dqalys_Dinutuximab_β"]])
            },
            "costs" = if(!discounted_output) {
              c(v_run_results[["costs_Isotretinoin"]],
                v_run_results[["costs_Dinutuximab_β"]])
            } else {
              c(v_run_results[["Dcosts_Isotretinoin"]],
                v_run_results[["Dcosts_Dinutuximab_β"]])
            }
          )
        }
      ) |>
        Reduce(
          x = _,
          f = rbind
        )
    }
  ) |>
    Reduce(
      x = _,
      f = rbind
    )

  # Process OWSA results:
  parameter <- NULL
  df_GD2_owsa_results <- base::subset(
    x = df_owsa_results,
    subset = (intervention == "Dinutuximab β")
  )
  df_TT_owsa_results <- base::subset(
    x = df_owsa_results,
    subset = (intervention == "Isotretinoin")
  )
  df_owsa_cea_results <- base::subset(
    x = df_owsa_results,
    subset = (intervention == "Isotretinoin"),
    select = names(df_owsa_results)[
      names(df_owsa_results) != c("intervention", "effects", "costs")]
  )
  # Estimate the required output:
  df_owsa_cea_results[["Isotretinoin Costs"]] <-
    df_TT_owsa_results[["costs"]]
  df_owsa_cea_results[["Isotretinoin QALYs"]] <-
    df_TT_owsa_results[["effects"]]
  df_owsa_cea_results[["Dinutuximab β Costs"]] <-
    df_GD2_owsa_results[["costs"]]
  df_owsa_cea_results[["Dinutuximab β QALYs"]] <-
    df_GD2_owsa_results[["effects"]]
  df_owsa_cea_results[["NMB"]] <-
    ((df_TT_owsa_results["effects"] * wtp - df_TT_owsa_results["costs"]) -
       (df_GD2_owsa_results["effects"] * wtp - df_GD2_owsa_results["costs"]))[, 1]
  df_owsa_cea_results["Differential Costs"] <-
    (df_TT_owsa_results["costs"] - df_GD2_owsa_results["costs"])[, 1]
  df_owsa_cea_results[["Differential QALYs"]] <-
    (df_TT_owsa_results["effects"] - df_GD2_owsa_results["effects"])[, 1]
  df_owsa_cea_results[["ICER"]] <-
    (df_TT_owsa_results["costs"] - df_GD2_owsa_results["costs"])[, 1] /
    (df_TT_owsa_results["effects"] - df_GD2_owsa_results["effects"])[, 1]

  return(df_owsa_cea_results)
}

#' Draw the One-Way Sensitivity Analysis (OWSA) - Tornado plot
#'
#' @param df_owsa Dataframe containing the names of the parameters included in
#' the One-Way Sensitivity Analysis (OWSA) in addition to the costs, effects,
#' Incremental Cost-Effectiveness Ratio (ICER) and Net Benefit (NB) values
#' resulting from evaluating the decision-analytic model using the corresponding
#' upper and lower values of the included parameters.
#' @param model_func A function to be used for the model, such as the
#' \code{\link{run_psm}}
#' @param model_func_args A list of model arguments and parameters that are not
#' varied in DSA
#' @param v_dsa_params A character vector of DSA parameter names.
#' @param l_dsa_params A list containing all model DSA parameters. The list is
#' expected to be either \code{\link{l_dsa_parameters}} or of identical
#' structure.
#' @param discounted_output A logical for whether to use discounted values.
#' Default is `FALSE`.
#' @param cea_metric A character scalar specifying the cost-effectiveness metric
#' to be used in the x-axis of the tornado plot. Supported options are `"NMB"`
#' (default), `"Differential Costs"`, `"Differential QALYs"` or `"ICER"`.
#' @param wtp Numeric scalar defining the cost-effectiveness threshold,
#' willingness-to-pay (WTP) value for the net monetary benefit calculation.
#' Default is `30000.`
#' @param parameters_labels Named character vector showing the labels of each of
#' the parameters listed in the `parameter_name_column` column in the `df_owsa`
#' dataframe. Default is `NULL`.
#' @param show_labels_in_caption Logical for whether to add a caption to the
#' plot showing the labels passed to `parameters_labels`. This argument is
#' ignored if `NULL` was passed to `parameters_labels`. Default is `FALSE`.
#' @param plot_title Character scalar representing the title of the plot.
#' Default is `"One-Way Sensitivity Analysis - Tornado plot"`.
#' @param plot_subtitle Character scalar representing the subtitle of the plot.
#' Default is `NULL`.
#' @param x_axis_label Character scalar to label the x-axis  of the plot.
#' Default is `"Incremental Net Benefit (iNMB)"`.
#' @param currency_symbol Character scalar representing the Hex code of the
#' currency symbol to be used as the y-axis-ticks' label. Default is `"$"`.
#' @param show_basecase_value_label Logical for whether to label the vertical
#' line representing the base-case ICER. Default is `TRUE`.
#' @param drop_insensitive Logical scalar specifying whether to remove form the
#' tornado plot parameters that have no effect on the CEA metric.
#' @param plot_bars_width Numeric scalar representing the width of the
#' horizontal colored bars. Default is `0.95` and the accepted values are
#' between `0.1` and `0.99`
#' @param plot_colors String vector of two color names or hex codes. The first
#' and second colors are assigned to the cost-effectiveness metric corresponding
#' to the lower and upper bound values of the assessed parameters, respectively.
#' Passing a named vector with the names `Upper` and `Lower` allows for better
#' control.
#'
#' @return A ggplot object depicting the sensitivity of the deterministic cost-
#' effectiveness results to changes in the values of the included parameters.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' owsa_NMB_plot <- NeuroblastomaPSM::plot_owsa(
#'    df_owsa = NeuroblastomaPSM::run_owsa(),
#'    model_func = NeuroblastomaPSM::run_psm,
#'    model_func_args = list(
#'        "models_fit" = NeuroblastomaPSM::parametric_models,
#'        "l_params" = c(
#'            time_horizon = 10,
#'            cycle_length = 1/12,
#'            disc_rate_costs = 0.035,
#'            disc_rate_qalys = 0.015,
#'            NeuroblastomaPSM::l_psm_parameters
#'          )
#'        ),
#'    v_dsa_params = names(NeuroblastomaPSM::l_dsa_parameters),
#'    l_dsa_params = NeuroblastomaPSM::l_dsa_parameters,
#'    discounted_output = FALSE,
#'    cea_metric = "NMB",
#'    wtp = 30000,
#'    parameters_labels = NULL,
#'    show_labels_in_caption = FALSE,
#'    plot_title = "One-Way Sensitivity Analysis - Tornado plot",
#'    plot_subtitle = NULL,
#'    x_axis_label = "Incremental Net Benefit",
#'    currency_symbol = "$",
#'    show_basecase_value_label = TRUE,
#'    plot_bars_width = 0.95,
#'    plot_colors = c("Lower" = "orange", "Upper" = "skyblue")
#' )
#' owsa_NMB_plot <- NeuroblastomaPSM::plot_owsa(
#'    df_owsa = NeuroblastomaPSM::run_owsa(),
#'    model_func = NeuroblastomaPSM::run_psm,
#'    model_func_args = list(
#'        "models_fit" = NeuroblastomaPSM::parametric_models,
#'        "l_params" = c(
#'            time_horizon = 10,
#'            cycle_length = 1/12,
#'            disc_rate_costs = 0.035,
#'            disc_rate_qalys = 0.015,
#'            NeuroblastomaPSM::l_psm_parameters
#'          )
#'        ),
#'    v_dsa_params = names(NeuroblastomaPSM::l_dsa_parameters),
#'    l_dsa_params = NeuroblastomaPSM::l_dsa_parameters,
#'    discounted_output = FALSE,
#'    cea_metric = "NMB",
#'    wtp = 30000,
#'    parameters_labels = NULL,
#'    show_labels_in_caption = FALSE,
#'    plot_title = "One-Way Sensitivity Analysis - Tornado plot",
#'    plot_subtitle = NULL,
#'    x_axis_label = "Incremental Net Benefit",
#'    currency_symbol = "$",
#'    show_basecase_value_label = TRUE,
#'    plot_bars_width = 0.95,
#'    plot_colors = c("Lower" = "orange", "Upper" = "skyblue")
#' )
#' owsa_DQALYs_plot <- NeuroblastomaPSM::plot_owsa(
#'    df_owsa = NeuroblastomaPSM::run_owsa(),
#'    model_func = NeuroblastomaPSM::run_psm,
#'    model_func_args = list(
#'        "models_fit" = NeuroblastomaPSM::parametric_models,
#'        "l_params" = c(
#'            time_horizon = 10,
#'            cycle_length = 1/12,
#'            disc_rate_costs = 0.035,
#'            disc_rate_qalys = 0.015,
#'            NeuroblastomaPSM::l_psm_parameters
#'          )
#'        ),
#'    v_dsa_params = names(NeuroblastomaPSM::l_dsa_parameters),
#'    l_dsa_params = NeuroblastomaPSM::l_dsa_parameters,
#'    discounted_output = FALSE,
#'    cea_metric = "Differential QALYs",
#'    wtp = 30000,
#'    parameters_labels = NULL,
#'    show_labels_in_caption = FALSE,
#'    plot_title = "One-Way Sensitivity Analysis - Tornado plot",
#'    plot_subtitle = NULL,
#'    x_axis_label = "Differential QALYs",
#'    currency_symbol = "$",
#'    show_basecase_value_label = TRUE,
#'    plot_bars_width = 0.95,
#'    plot_colors = c("Lower" = "orange", "Upper" = "skyblue")
#' )
#' owsa_Dcosts_plot <- NeuroblastomaPSM::plot_owsa(
#'    df_owsa = NeuroblastomaPSM::run_owsa(),
#'    model_func = NeuroblastomaPSM::run_psm,
#'    model_func_args = list(
#'        "models_fit" = NeuroblastomaPSM::parametric_models,
#'        "l_params" = c(
#'            time_horizon = 10,
#'            cycle_length = 1/12,
#'            disc_rate_costs = 0.035,
#'            disc_rate_qalys = 0.015,
#'            NeuroblastomaPSM::l_psm_parameters
#'          )
#'        ),
#'    v_dsa_params = names(NeuroblastomaPSM::l_dsa_parameters),
#'    l_dsa_params = NeuroblastomaPSM::l_dsa_parameters,
#'    discounted_output = FALSE,
#'    cea_metric = "Differential Costs",
#'    wtp = 30000,
#'    parameters_labels = NULL,
#'    show_labels_in_caption = FALSE,
#'    plot_title = "One-Way Sensitivity Analysis - Tornado plot",
#'    plot_subtitle = NULL,
#'    x_axis_label = "Differential Costs",
#'    currency_symbol = "$",
#'    show_basecase_value_label = TRUE,
#'    plot_bars_width = 0.95,
#'    plot_colors = c("Lower" = "orange", "Upper" = "skyblue")
#' )
#' owsa_ICER_plot <- NeuroblastomaPSM::plot_owsa(
#'    df_owsa = NeuroblastomaPSM::run_owsa(),
#'    model_func = NeuroblastomaPSM::run_psm,
#'    model_func_args = list(
#'        "models_fit" = NeuroblastomaPSM::parametric_models,
#'        "l_params" = c(
#'            time_horizon = 10,
#'            cycle_length = 1/12,
#'            disc_rate_costs = 0.035,
#'            disc_rate_qalys = 0.015,
#'            NeuroblastomaPSM::l_psm_parameters
#'          )
#'        ),
#'    v_dsa_params = names(NeuroblastomaPSM::l_dsa_parameters),
#'    l_dsa_params = NeuroblastomaPSM::l_dsa_parameters,
#'    discounted_output = FALSE,
#'    cea_metric = "ICER",
#'    wtp = 30000,
#'    parameters_labels = NULL,
#'    show_labels_in_caption = FALSE,
#'    plot_title = "One-Way Sensitivity Analysis - Tornado plot",
#'    plot_subtitle = NULL,
#'    x_axis_label = "Incremental Cost Effectiveness Ratio (ICER)",
#'    currency_symbol = "$",
#'    show_basecase_value_label = TRUE,
#'    plot_bars_width = 0.95,
#'    plot_colors = c("Lower" = "orange", "Upper" = "skyblue")
#' )
#' }
plot_owsa <- function(
    df_owsa = NeuroblastomaPSM::run_owsa(),
    model_func = NeuroblastomaPSM::run_psm,
    model_func_args = list(
      "models_fit" = NeuroblastomaPSM::parametric_models,
      "l_params" = c(
        time_horizon = 10,
        cycle_length = 1/12,
        disc_rate_costs = 0.035,
        disc_rate_qalys = 0.015,
        NeuroblastomaPSM::l_psm_parameters
      )
    ),
    v_dsa_params = names(NeuroblastomaPSM::l_dsa_parameters),
    l_dsa_params = NeuroblastomaPSM::l_dsa_parameters,
    discounted_output = FALSE,
    cea_metric = "NMB",
    wtp = 30000,
    parameters_labels = NULL,
    show_labels_in_caption = FALSE,
    plot_title = "One-Way Sensitivity Analysis - Tornado plot",
    plot_subtitle = NULL,
    x_axis_label = "Incremental Net Benefit (iNMB)",
    currency_symbol = "$",
    show_basecase_value_label = TRUE,
    drop_insensitive = TRUE,
    plot_bars_width = 0.95,
    plot_colors = c("Lower" = "orange", "Upper" = "skyblue")) {

  # Run model using base-case assumptions:
  v_base_case_run_results <- do.call(
    what = model_func,
    args = model_func_args
  )

  df_base_case <- data.frame(
    "intervention" = c("Isotretinoin", "Dinutuximab β"),
    "effects" = if(!discounted_output) {
      c(v_base_case_run_results[["qalys_Isotretinoin"]],
        v_base_case_run_results[["qalys_Dinutuximab_β"]])
    } else {
      c(v_base_case_run_results[["Dqalys_Isotretinoin"]],
        v_base_case_run_results[["Dqalys_Dinutuximab_β"]])
    },
    "costs" = if(!discounted_output) {
      c(v_base_case_run_results[["costs_Isotretinoin"]],
        v_base_case_run_results[["costs_Dinutuximab_β"]])
    } else {
      c(v_base_case_run_results[["Dcosts_Isotretinoin"]],
        v_base_case_run_results[["Dcosts_Dinutuximab_β"]])
    }
  )

  df_base_case[["NMB"]] <-
    ((df_base_case[1, "effects"] * wtp - df_base_case[1, "costs"]) -
       (df_base_case[2, "effects"] * wtp - df_base_case[2, "costs"]))
  df_base_case["Differential Costs"] <-
    (df_base_case[1, "costs"] - df_base_case[2, "costs"])
  df_base_case[["Differential QALYs"]] <-
    (df_base_case[1, "effects"] - df_base_case[2, "effects"])
  df_base_case[["ICER"]] <-
    (df_base_case[1, "costs"] - df_base_case[2, "costs"]) /
    (df_base_case[1, "effects"] - df_base_case[2, "effects"])
  df_base_case <- df_base_case[1, c("NMB", "Differential Costs",
                                    "Differential QALYs", "ICER")]

  # Enforce expected names for parameters, upper and lower bound CEA metric:
  df_owsa_plot <- NULL
  df_owsa_plot[["parameter_name"]] <- df_owsa[
    df_owsa$ci_bound_label == "upper_ci", "parameter"
  ]
  df_owsa_plot$Upper <- df_owsa[
    df_owsa$ci_bound_label == "upper_ci", cea_metric
  ]
  df_owsa_plot$Lower <- df_owsa[
    df_owsa$ci_bound_label == "lower_ci", cea_metric
  ]
  df_owsa_plot <- as.data.frame(df_owsa_plot)

  # Order parameters based on CEA metric sensitivity:
  df_owsa_plot[["cea_range"]] <- abs(df_owsa_plot$Lower - df_owsa_plot$Upper)
  df_owsa_plot <- df_owsa_plot[order(df_owsa_plot$cea_range), ]
  if(drop_insensitive) {
    df_owsa_plot <- df_owsa_plot[df_owsa_plot$cea_range != 0, ]
  }
  parameters_order <- df_owsa_plot$parameter_name
  df_owsa_plot$cea_range <- NULL

  # Get the data frame in shape plotting:
  df_owsa_long <- df_owsa_plot |>
    stats::reshape(
      data = _,
      v.names = cea_metric,
      timevar = "Parameter Bound",
      time = c("Lower", "Upper"),
      direction = "long",
      varying =  c("Lower", "Upper"),
      idvar = "parameter_name",
      new.row.names = NULL
    ) |>
    `rownames<-`(
      x = _,
      NULL
    )
  df_owsa_long$parameter_name <- factor(
    x = df_owsa_long$parameter_name,
    levels = parameters_order
  )
  df_owsa_long <- cbind(
    df_owsa_long,
    "y_min" = pmin(df_owsa_long[[cea_metric]], df_base_case[[cea_metric]]),
    "y_max" = pmax(df_owsa_long[[cea_metric]], df_base_case[[cea_metric]]),
    "x_min" = as.numeric(df_owsa_long$parameter_name) - plot_bars_width/2,
    "x_max" = as.numeric(df_owsa_long$parameter_name) + plot_bars_width/2
  )

  # Draw the OWSA tornado plot:
  `Parameter Bound` <- x_max <- x_min <- y_max <- y_min <- NULL
  plot_owsa <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = df_owsa_long,
      ggplot2::aes(
        ymax = y_max,
        ymin = y_min,
        xmax = x_max,
        xmin = x_min,
        fill = `Parameter Bound`
      ),
      alpha = 0.5
    ) +
    ggplot2::scale_fill_manual(
      breaks = c("Lower", "Upper"),
      values = plot_colors
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(1:length(parameters_order)),
      labels = if(isTRUE(show_labels_in_caption)) {
        parameters_order
      } else {
        if (!is.null(parameters_labels)) {
          parameters_labels[parameters_order]
        } else {
          parameters_order
        }
      }
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      y = x_axis_label
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.subtitle = if(is.null(plot_subtitle)) ggplot2::element_blank()
      else ggplot2::element_text(),
      axis.title.y = ggplot2::element_blank(),
      legend.position = 'top'
    )

  # Apply currency label where applicable:
  if(isTRUE(cea_metric != "Differential QALYs")) {
    plot_owsa <- plot_owsa +
    ggplot2::scale_y_continuous(
      labels = scales::label_dollar(prefix = currency_symbol, big.mark = ",")
    )
  }

  # Label the base-case ICER vertical line:
  if(isTRUE(show_basecase_value_label)) {
    plot_owsa <- plot_owsa +
      ggplot2::geom_hline(
        yintercept = df_base_case[[cea_metric]]
      ) +
      ggplot2::geom_label(
        mapping = ggplot2::aes(
          x = df_owsa_long$x_max[nrow(df_owsa_long)] + plot_bars_width/2,
          y = df_base_case[[cea_metric]],
          label = paste0(
            "Base-case ", cea_metric, " = ",
            ifelse(cea_metric != "Differential QALYs", currency_symbol, ""),
            format(x = df_base_case[[cea_metric]], big.mark = ",")
          )
        )
      )
  }

  # Add a caption with longer labels (descriptions) for the parameters:
  if(all(isTRUE(show_labels_in_caption), !is.null(parameters_labels))) {
    parameters_labels <- parameters_labels[parameters_order]

    plot_owsa <- plot_owsa +
      ggplot2::labs(
        caption =
          paste0(
            names(parameters_labels), ": ", parameters_labels, ".",
            collapse = " "
          )
      ) +
      ggplot2::theme(
        plot.caption = ggtext::element_textbox_simple()
      )
  }

  return(plot_owsa)
}
