#' Plots the results of the parameter recovery attempt
#'
#' @inheritParams itchmodel::fit_model
#' @param orig_params named vector of with original parameter values
#' @param optim_output output of the Differential Evolution optimization routine
plot_parameter_recovery_results <- function(model = "DDM",
                                            parameterization = "time_sensitivity",
                                            orig_params,
                                            optim_output
                                            ) {

  bounds_and_origparam <-
    tibble::tibble("parameter" = itchmodel::get_par_names(model = model,
                                                          parameterization = parameterization),
                   "lower" = optim_output$member$lower,
                   "upper" = optim_output$member$upper,
                   "origparam" = unlist(orig_params)
    )

  par_vals <- optim_output$member$bestmemit
  colnames(par_vals) <- itchmodel::get_par_names(model = "DDM",
                                                 parameterization = parameterization)
  par_vals <-
    tibble::as.tibble(par_vals) %>%
    dplyr::mutate(iter = 1:nrow(.)) %>%
    tidyr::gather(key = "parameter", value = "value", -iter)

  plot_data <-
    par_vals %>%
    dplyr::left_join(bounds_and_origparam, by = "parameter")

  ggplot2::ggplot(data = plot_data,
                  ggplot2::aes(x = iter,
                               y = value)
  ) +
    ggplot2::facet_wrap("parameter", scales = "free_y") +
    ggplot2::geom_line() +

    ggplot2::geom_hline(ggplot2::aes(yintercept = lower), color = "red") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = upper), color = "red") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = origparam), color = "blue", linetype = "dotted") +
    ggplot2::scale_x_continuous(name = "Iteration") +
    ggplot2::scale_y_continuous(name = "Value")

}

#' Plots the evolution of the optimization function value across iterations
#'
#' @inheritParams plot_parameter_recovery_results

plot_optim_fun_evol <- function(optim_output) {

  fun_vals <-
    tibble::as.tibble(optim_output$member$bestvalit) %>%
    dplyr::mutate(iter = 1:nrow(.))

  ggplot2::ggplot(data = fun_vals,
                  ggplot2::aes(x = iter,
                               y = value)
                  ) +
    ggplot2::geom_line() +

    ggplot2::scale_x_continuous(name = "Iteration") +
    ggplot2::scale_y_continuous(name = "Function value")

}
