p_ll_fun_of_t_s <- function(x, m_l, m_s, t_l, parameters) {

  pars <- unlist(parameters)

  d = pars['w'] * (pars['mu'] * m_l^pars['alpha'])

}


#' Plots the results of the parameter recovery attempt
#'
#' @inheritParams itchmodel::fit_model
#' @param orig_params named vector of with original parameter values
#' @param optim_output output of the Differential Evolution optimization routine
plot_parameter_recovery_results <- function(model = "DDM",
                                            parameterization = "defer_speedup_time_scaling",
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


plot_transformation <- function(df, nested_parameters, what = 'time', mx = 100) {

  plt <-
    ggplot2::ggplot(df, ggplot2::aes(x = x,
                                     y = y,
                                     color = frame,
                                     shape = frame)
                    )


  for (i_frame in 1:nrow(nested_parameters)) {
    if (what == 'money') {
      plt <-
        plt +
        ggplot2::stat_function(ggplot2::aes(x),
                               fun = function(x,p) {p['mu'] * x^p['alpha']},
                               args = list(p = unlist(nested_parameters$parameters[[i_frame]])),
                               color = 'black')
    } else if (what == 'time') {
      plt <-
        plt +
        ggplot2::stat_function(ggplot2::aes(x),
                               fun = function(x,p) {p['kappa'] * x^p['beta']},
                               args = list(p = unlist(nested_parameters$parameters[[i_frame]])),
                               color = 'black')
    }
  }

  plt <-
    plt +
    ggplot2::geom_point(stroke = 1)

  if (what == 'money') {
    plt <-
      plt +

      ggplot2::scale_x_continuous(name = 'Monetary amount',
                                  labels = scales::dollar_format(suffix = "", prefix = "€"),
                                  limits = c(0,mx)) +
      ggplot2::scale_y_continuous(name = 'Utility (a.u.)')
  } else if (what == 'time') {
    plt <-
      plt +
      ggplot2::scale_x_continuous(name = 'Clock time (days)',
                                  limits = c(0,mx)) +
      ggplot2::scale_y_continuous(name = 'Perceived time (a.u.)')
  }

  plt <-
    plt +
    ggplot2::scale_shape_manual(values = c(3, 4, 1)) +

    ggplot2::theme(legend.position = 'none')
    # ggplot2::theme(legend.position = c(0.8, 0.3))

  # plt <-
  #   plt +
  #   ggplot2::coord_equal()

  plt
}

#'
#'
#'
plt_rt_dist <- function(df, type = 'pdf') {

  rt_summary <-
    df %>%
    dplyr::group_by(frame, response) %>%
    dplyr::summarize(mean_rt = mean(rt))

  plt <-
    ggplot2::ggplot(df,
                    ggplot2::aes(x = rt,
                                 color = frame,
                                 linetype = response
                                 )
                    )

    if (type == 'pdf') {
      plt <-
        plt +

        ggplot2::geom_density() +

        ggplot2::geom_vline(data = rt_summary,
                            ggplot2::aes(xintercept = mean_rt,
                                         color = frame,
                                         linetype = response))

    } else if (type == 'cdf') {
      plt <-
        plt +
        ggplot2::stat_ecdf()
    }

  plt <-
    plt + ggplot2::theme(legend.position = 'none')

  plt
}

plot_model_parameters <- function(stimuli, parameters, parameterization = "date_delay_time_scaling",
                                  m = c(10,20), t = c(2, 9), m_max = 100, t_max = 100) {

  nested_stimuli_parameters <-
    itchmodel::nest_join_stimuli_parameters(stimuli = stimuli,
                                            parameters = parameters,
                                            parameterization = parameterization)

  # Plot transformation of money to utility ------------------------------------

  tibble::tibble(frame = factor(),
                 x = double(),
                 u = double()) %>%
    tibble::add_row(u = purrr::pmap(.l = list(parameters = nested_stimuli_parameters$parameters,
                                          frame = nested_stimuli_parameters$frame
                                          ),
                                .f = itchmodel::compute_transformation,
                                q = m[1],
                                variable = 'u_ss'
                                ))


  m2u <-
    nested_stimuli_parameters %>%

    # Compute utility for the SS option, given monetary amount, model parameters and frame
    dplyr::mutate(m_ss = list(m[1]),
                  u_ss = purrr::pmap(.l = list(parameters = nested_stimuli_parameters$parameters,
                                               frame = as.character(nested_stimuli_parameters$frame)),
                                     .f = itchmodel::compute_transformation,
                                     q = m[1], variable = 'u_ss')) %>%

    # Compute utility for the LL option, given monetary amount, model parameters and frame
    dplyr::mutate(m_ll = list(m[2]),
                  u_ll = purrr::pmap(.l = list(parameters = nested_stimuli_parameters$parameters,
                                               frame = as.character(nested_stimuli_parameters$frame)),
                                     .f = itchmodel::compute_transformation,
                                     q = m[2], variable = 'u_ll')) %>%
    # Make tibble with columns frame, x (monetary amount), and u
    dplyr::select(-parameters, -stimuli) %>%
    tidyr::unnest() %>%
    tidyr::gather(key = "key", value = "value", -frame) %>%
    tidyr::extract(col = key,
                   into = c('variable', 'option'),
                   regex = "([[:alnum:]]+)_([[:alnum:]]+)") %>%
    tidyr::spread(variable, value) %>%
    dplyr::rename(x = m,
                  y = u)  %>%
    dplyr::select(-option)

  # Plot it
  plt_m2u <-
    plot_transformation(df = m2u,
                        nested_parameters = nested_stimuli_parameters,
                        what = 'money',
                        mx = m_max)

  # Plot transformation of clock time to weighted/perceived time ---------------

  t2p <-
    nested_stimuli_parameters %>%
    # Compute weighted time for the SS option, given clock time, model parameters and frame
    dplyr::mutate(t_ss = list(t[1]),
                  p_ss = purrr::pmap(.l = list(parameters = nested_stimuli_parameters$parameters,
                                               frame = as.character(nested_stimuli_parameters$frame)),
                                     .f = itchmodel::compute_transformation,
                                     q = t[1], variable = 'p_ss')) %>%

    # Compute weighted time for the LL option, given clock time, model parameters and frame
    dplyr::mutate(t_ll = list(t[2]),
                  p_ll = purrr::pmap(.l = list(parameters = nested_stimuli_parameters$parameters,
                                               frame = as.character(nested_stimuli_parameters$frame)),
                                     .f = itchmodel::compute_transformation,
                                     q = t[2], variable = 'p_ll')) %>%

    # Make tibble with columns frame, x (clock time), and p
    dplyr::select(-parameters, -stimuli) %>%
    tidyr::unnest() %>%
    tidyr::gather(key = "key", value = "value", -frame) %>%
    tidyr::extract(col = key,
                   into = c('variable', 'option'),
                   regex = "([[:alnum:]]+)_([[:alnum:]]+)") %>%
    tidyr::spread(variable, value) %>%
    dplyr::rename(x = t,
                  y = p)  %>%
    dplyr::select(-option)

  # Plot it
  plt_t2p <-
    plot_transformation(df = t2p,
                        nested_parameters = nested_stimuli_parameters,
                        what = 'time',
                        mx = t_max)

  # Plot weighted utilities, perceived times, and drift rates ------------------


  transformation_diffs <-
    nested_stimuli_parameters %>%
      dplyr::mutate(du_dp_w_v = purrr::pmap(.l = list(stimuli = .$stimuli,
                                                      parameters = .$parameters,
                                                      frame = as.character(.$frame)),
                                       .f = itchmodel::compute_drift_rate_transformation_diffs,
                                       parameterization = parameterization)
                    ) %>%
      dplyr::select(-stimuli, -parameters) %>%
      tidyr::unnest()

  plt_transformation_diffs <-
    plot_transformation_diffs(transformation_diffs)


  # Plot response probabilities ------------------------------------------------

  # Probability of choosing LL, when only varying m_s
  stimuli_p_ll_of_m_s <-
    itchmodel::make_stimulus_df(frames = itchmodel::get_frames(parameterization),
                                m_l = m[2],
                                pct_m_l = unique(c(seq(from = 0.005, to = 1, by = 0.005), m[1]/m[2])),
                                t_s = t[1],
                                interval = t[2] - t[1],
                                n_reps = 1
    )

  p_ll_of_m_s <-
    itchmodel::sim_data(stimuli = stimuli_p_ll_of_m_s,
                        parameters = parameters,
                        parameterization = parameterization,
                        n = 1,
                        sim_fun = 'dai_busemeyer') %>%
    dplyr::select(model, dplyr::everything(), -parameters) %>%
    tidyr::unnest()

  plt_p_ll_of_m_s <-
    ggplot2::ggplot(data = p_ll_of_m_s,
                    ggplot2::aes(x = m_s,
                                 y = p_ll,
                                 color = frame)) +
    ggplot2::geom_line() +

    ggplot2::geom_point(data = p_ll_of_m_s %>% dplyr::filter(m_s == m[1]),
                        ggplot2::aes(shape = frame),
                        stroke = 1) +

    ggplot2::scale_x_continuous(name = 'Monetary amount SS option',
                                labels = scales::dollar_format(suffix = "", prefix = "€"),
                                limits = c(0,m_max)) +

    ggplot2::scale_shape_manual(values = c(3, 4, 1)) +

    ggplot2::expand_limits(x = 0, y = 0) +
    ggplot2::theme(legend.position = 'none')

  # Probability of choosing LL, when only varying t_l
  stimuli_p_ll_of_t_l <-
    itchmodel::make_stimulus_df(frames = itchmodel::get_frames(parameterization),
                                m_l = m[2],
                                pct_m_l = m[1] / m[2],
                                t_s = t[1],
                                interval = seq(from = 0, to = t_max, by = 0.25),
                                n_reps = 1
    )

  p_ll_of_t_l <-
    itchmodel::sim_data(stimuli = stimuli_p_ll_of_t_l,
                        parameters = parameters,
                        parameterization = parameterization,
                        n = 1,
                        sim_fun = 'dai_busemeyer') %>%
    dplyr::select(model, dplyr::everything(), -parameters) %>%
    tidyr::unnest()

  plt_p_ll_of_t_l <-
    ggplot2::ggplot(data = p_ll_of_t_l,
                    ggplot2::aes(x = t_l,
                                 y = p_ll,
                                 color = frame)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(data = p_ll_of_t_l %>% dplyr::filter(t_l == t[2]),
                        ggplot2::aes(shape = frame),
                        stroke = 1) +

    ggplot2::scale_x_continuous(name = 'Clock time or delay LL option',
                              limits = c(0, t_max)) +

    ggplot2::scale_shape_manual(values = c(3, 4, 1)) +

    ggplot2::expand_limits(x = 0, y = 0) +
    ggplot2::theme(legend.position = 'none')



  # Plot RT distribution -------------------------------------------------------

  rt_data <-
    sim_data(stimuli = stimuli,
             parameters = parameters,
             parameterization = parameterization,
             n = 1000) %>%
    dplyr::select(model, frame, observations) %>%
    tidyr::unnest()

  plt_rt <-
    plt_rt_dist(df = rt_data, type = 'cdf')

  # Plot subplot in a grid -----------------------------------------------------
  cowplot::plot_grid(plt_m2u, plt_t2p, plt_transformation_diffs,
                     plt_p_ll_of_t_l, plt_p_ll_of_m_s, plt_rt)

}


plot_transformation_diffs <- function(df) {
  ggplot2::ggplot(df,
                  ggplot2::aes(x = variable,
                               y = value,
                               color = frame,
                               shape = frame)) +
    ggplot2::facet_wrap('weighted') +
    ggplot2::geom_point(stroke = 1) +
    ggplot2::scale_shape_manual(values = c(3, 4, 1)) +
    ggplot2::expand_limits(y = 0)
}


## plot_model_fit_to_data ######################################################
#' This function takes data, best-fitting parameters and plots empirical observation and model predictions
#'
#' @param obs observations, a tibble containing stimuli and responses
#' @param params parameters, vector
#' @param model model name
#' @param parameterization model parameterization
#'
plot_model_fit_to_data <- function(df, params, model_name = "", parameterization = "") {



  # Add model parameters =======================================================
  df <-
    df %>%
    dplyr::mutate(parameters = itchmodel::get_par_values(params,
                                                         model = model_name,
                                                         parameterization = parameterization)
    )


  stim <- df$stimuli[[1]]

  stimuli_for_prd <-
    tidyr::crossing(frame = df$frame,
                    # m_s = max(stim$m_l) * seq(from = 2^-9,
                    #                           to = 1,
                    #                           by = 2^-9),
                    m_s = max(stim$m_l) * seq(from = 2^-4,
                                              to = 1,
                                              by = 2^-4),
                    t_s = 0,
                    m_l = max(stim$m_l),
                    t_l = unique(stim$t_l)
                    # t_l = factor(unique(stim$t_l), levels = unique(stim$t_l))
                    ) %>%
    dplyr::group_by(frame) %>%
    tidyr::nest(.key = stimuli)


  df <-
    df %>%
    dplyr::mutate(predictions = purrr::pmap(.l = list(stim = stimuli_for_prd$stimuli,
                                                      x = .$parameters,
                                                      frame = .$frame),
                                            .f = get_model_predictions,
                                            parameterization = parameterization
                                            )
                  )

  # Summarize stimuli and observations =========================================
  df <-
    df %>%
    dplyr::mutate(stimuli_sum = purrr::pmap(.l = list(stim = .$stimuli),
                                            .f = summarize_stimuli),
                  observations_sum = purrr::pmap(.l = list(stim = .$stimuli,
                                                           obs = .$observations),
                                                 .f = summarize_observations)
                  # predictions_sum = purrr::pmap(.l = list(stim = .$stimuli,
                  #                                         prd = .$predictions),
                  #                               .f = summarize_predictions)
                  )

  # Plot =======================================================================

  obs_sum <-
    dplyr::select(df, frame, observations_sum) %>%
    tidyr::unnest()

  prds <-
    dplyr::select(df, frame, predictions) %>%
    tidyr::unnest()

  # m_l <- 43.52
  # t_s <- 0

  plt <-
    ggplot2::ggplot(obs_sum,
                    ggplot2::aes(x = m_s,
                                 y = p_ll,
                                 color = frame,
                                 shape = frame)
    ) +
    ggplot2::facet_wrap("t_l")

  plt <-
    plt +
    ggplot2::geom_line(data = prds)

  plt <-
    plt +
    ggplot2::geom_point(stroke = 1) +
    ggplot2::scale_shape_manual(values = c(3, 4, 1)) +
    ggplot2::scale_x_continuous(name = "Small amount (Euro)") +
    ggplot2::scale_y_continuous(name = "P(LL)") +
    ggplot2::theme_minimal()

  plt
}


# Subfunctions =================================================================

#' Get stimuli summary statistics
#'
#' @param stim Stimuli
summarize_stimuli <- function(stim) {
  stim %>%
    dplyr::group_by(t_l, m_s_cat) %>%
    dplyr::summarize(m_s = mean(m_s),
                     m_l = mean(m_l)
    )
}

#' Get observation summary statistics
#'
#' @param stim Stimuli
#' @param obs Observations
summarize_observations <- function(stim, obs) {
  dplyr::bind_cols(stim, obs) %>%
    dplyr::mutate(response_int = dplyr::recode(.$response,
                                               "upper" = 1,
                                               "lower" = 0)
    ) %>%
    dplyr::group_by(t_l, m_s_cat) %>%
    dplyr::summarize(m_s = mean(m_s),
                     p_ll = mean(response_int),
                     mean_rt = mean(rt)
    )
}

#' Get observation summary statistics of predictions
#'
#' @param stim Stimuli
#' @param obs Observations
summarize_predictions <- function(stim, prd) {
  dplyr::bind_cols(stim, prd) %>%
    dplyr::group_by(t_l, m_s_cat) %>%
    dplyr::summarize(m_s = mean(m_s),
                     p_ll = mean(p_ll),
                     mean_rt = mean(rt)
    )
}

#' Get diffusion model predictions
#'
#' @param x vector of parameter values
#' @param stim Stimuli
#' @param frame Framing
#' @param parameterization Parameterization of the model
get_model_predictions <- function(x, stim, frame = "", parameterization = "") {

  v <-
    compute_drift_rate(parameters = x,
                       stimuli = stim,
                       parameterization = parameterization,
                       frame = frame)


  # v <-
  #   itchmodel::itch_ddm(stimuli = stim,
  #                       parameters = x,
  #                       parameterization = parameterization,
  #                       frame = frame,
  #                       sim_fun = "rtdists_package") %>%
  #   dplyr::pull(v)

  # Let's do this using Dai & Busemeyer's method, which gives identical probabilities as rtdists::pdiffusion, but
  # is faster
  p_ll <- itchmodel::db_bin_choice_prob(d = v,
                                        s = 1,
                                        a = x["a"],
                                        z = 0)


  # p_ll <-
  #   rtdists::pdiffusion(rt = rep(Inf, length(v)),
  #                       response = "upper",
  #                       a = x['a'],
  #                       v = v,
  #                       t0 = x['t0'])

  # The RT predictions need to be double-checked, not sure they'tre correct.
  # At least, approach seems in line with the vignette:
  # https://cran.r-project.org/web/packages/rtdists/vignettes/reanalysis_rr98.html#predicted-median-rts

  # rt <-
  #   rtdists::qdiffusion(p = p_ll,
  #                       response = "upper",
  #                       a = x['a'],
  #                       v = v,
  #                       t0 = x['t0'],
  #                       maxt = 30,
  #                       interval = c(0, 30))

  # rt <- purrr::pmap_dbl(.l = list(v = v),
  #                       .f = function(n, v, a, t0) {
  #                         mean(rtdists::rdiffusion(n = n,
  #                                             v = v,
  #                                             a = a,
  #                                             t0 = t0)$rt,
  #                              na.rm = TRUE)},
  #                       n = 30,
  #                       a = x['a'],
  #                       t0 = x['t0']
  #                       )

  stim %>%
    dplyr::mutate(v = v,
                  p_ll = p_ll)

}
