#' Compute drift rate using approaches by Dai & Busemeyer (2014) and Scholten & Read (2013)
#'
#'
#' @param parameters parameters
#' @param stimuli stimuli
#' @param frame the way in which the choice was framed: "defer", "speedup", "date", "delay
#' @inheritParams itchmodel::fit_model
#' @export
compute_drift_rate <- function(parameters, stimuli, parameterization = "", frame = "") {

    x <- unlist(parameters)

    # In general, drift rate is computed according to Dai & Busemeyer, J Exp Psychol Gen, 2014
    # v = x['w'] * (um_l - um_s) - (1 - x['w']) * (pt_l - pt_s)
    #
    # In case of the defer-speedup effect under outcome framing, we follow Scholten & Read, J Exp Psychol Learn Mem Cogn, 2013 (p. 1197)


    if (parameterization == "defer_speedup_value_scaling") {
      if (frame == "neutral") {
        x['w'] *
          (convert(stimuli$m_l, type = 'power', param = c(x['alpha'], x['mu'])) - # um_s
             convert(stimuli$m_s, type = 'power', param = c(x['alpha'], x['mu']))) - # um_s
          (1 - x['w']) *
          (convert(stimuli$t_l, type = 'power', param = c(x['beta'], x['kappa'])) - # pt_l
             convert(stimuli$t_s, type = 'power', param = c(x['beta'], x['kappa']))) # pt_s
      } else if (frame == "defer") {
        # v = x['w'] * (um_l - um_s) - (1 - x['w']) * (pt_l - pt_s)
        # See Dai & Busemeyer, J Exp Psychol Gen, 2014
        x['w'] *
          (convert(stimuli$m_l, type = 'power', param = c(x['alpha'], 1)) - # um_l
             convert(stimuli$m_s, type = 'power', param = c(x['alpha'], x['mu']))) - # um_s
          (1 - x['w']) *
          (convert(stimuli$t_l, type = 'power', param = c(x['beta'], x['kappa'])) - # pt_l
             convert(stimuli$t_s, type = 'power', param = c(x['beta'], x['kappa']))) # pt_s

      } else if (frame == "speedup") {

        x['w'] *
          (convert(stimuli$m_l, type = 'power', param = c(x['alpha'], x['mu'])) - # um_l
             convert(stimuli$m_s, type = 'power', param = c(x['alpha'], 1))) - # um_s
          (1 - x['w']) *
          (convert(stimuli$t_l, type = 'power', param = c(x['beta'], x['kappa'])) - # pt_l
             convert(stimuli$t_s, type = 'power', param = c(x['beta'], x['kappa']))) # pt_s
      }
    } else {

      x['w'] *
        (convert(stimuli$m_l, type = 'power', param = c(x['alpha'], x['mu'])) - # um_s
           convert(stimuli$m_s, type = 'power', param = c(x['alpha'], x['mu']))) - # um_s
        (1 - x['w']) *
        (convert(stimuli$t_l, type = 'power', param = c(x['beta'], x['kappa'])) - # pt_l
           convert(stimuli$t_s, type = 'power', param = c(x['beta'], x['kappa']))) # pt_s
    }

}

#' Convert a quantity into another according to a transformation function
#'
#' @param q quantity (either money in Euros or time in days)
#' @param type transformation function
#' @param params transformation function parameters
#' @export
convert <- function(q, type = 'power', param) {
  # Assertions
  # assertthat::assert_that(is.vector(q) | is.data.frame(q))
  # assertthat::assert_that(is.numeric(q))
  assertthat::assert_that(is.character(type))
  assertthat::assert_that(is.vector(param))
  assertthat::assert_that(length(param) == 2)
  assertthat::assert_that(is.numeric(param))

  switch(type,
         power = param[2] * q^param[1],

         # TODO: This needs to be checked against Scholten & Read (2013)
         log = param[2] * (1 / param[1] * log(1 + param[1] * q))
  )
}

#' Function for fitting intertemporal choice diffusion model
#'
#' @param data Tibble with nested data
#' @param model Name of the model. Currently, only DDM is supported.
#' @param parameterization Name of the parameterization. Currently, there are five possibilities: (1) date_delay_value_sensitivity; (2) date_delay_time_scaling; (3) defer_speedup_value_scaling; (4) defer_speedup_time_scaling, and (5) one_condition.
#' @param control A list of control parameters for the Differential Evolution algorithm
fit_model <- function(data,
                      model = "DDM",
                      parameterization = "date_delay_time_scaling",
                      lowers = get_par_bounds(model = "DDM", parameterization = "date_delay_time_scaling", bound = "lower"),
                      uppers = get_par_bounds(model = "DDM", parameterization = "date_delay_time_scaling", bound = "upper"),
                      control = list(itermax = 250,
                                     reltol = 1e-5)
                      ) {

  # Use Differential Evolution to optimize model parameters
  tmp <-
    DEoptim::DEoptim(fn = itchmodel::get_log_likelihood,
                     lower = lowers,
                     upper = uppers,
                     data = data,
                     model = model,
                     parameterization = parameterization,
                     control = control
                     )

  attributes(tmp)$model = model
  attributes(tmp)$parameterisation = parameterization
  tmp

}

#' Get the log-likelihood of the parameters, given the data
#'
#' This function is a wrapper around ll_diffusion.
#'
#' @param x parameters
#' @inheritParams fit_model
#' @export
get_log_likelihood = function(x, data, model = "DDM", parameterization = "") {

  parameter_names <- get_par_names(model = model, parameterization = "one_condition")
  params = list()

  names(x) = get_par_names(model = model, parameterization = parameterization)

  if (parameterization == "one_condition") {
    params = c(x["alpha"], x["mu"], x["beta"], x["kappa"], x["w"], x["a"], x["t0"])
    names(params) = parameter_names
  } else if (parameterization == "date_delay_value_sensitivity") {
    params[[1]] = c(x["alpha1"], x["mu"], x["beta"], x["kappa"], x["w"], x["a"], x["t0"])
    params[[2]] = c(x["alpha2"], x["mu"], x["beta"], x["kappa"], x["w"], x["a"], x["t0"])
    names(params[[1]]) = names(params[[2]]) = parameter_names
  } else if (parameterization == "date_delay_time_scaling") {
    params[[1]] = c(x["alpha"], x["mu"], x["beta"], x["kappa1"], x["w"], x["a"], x["t0"])
    params[[2]] = c(x["alpha"], x["mu"], x["beta"], x["kappa2"], x["w"], x["a"], x["t0"])
    names(params[[1]]) = names(params[[2]]) = parameter_names
  } else if (parameterization == "defer_speedup_value_scaling") {
    params[[1]] = c(x["alpha"], x["mu1"], x["beta"], x["kappa"], x["w"], x["a"], x["t0"])
    params[[2]] = c(x["alpha"], x["mu2"], x["beta"], x["kappa"], x["w"], x["a"], x["t0"])
    params[[3]] = c(x["alpha"], x["mu2"], x["beta"], x["kappa"], x["w"], x["a"], x["t0"])
    names(params[[1]]) = names(params[[2]]) = names(params[[3]]) = parameter_names
  } else if (parameterization == "defer_speedup_time_scaling") {
    params[[1]] = c(x["alpha"], x["mu"], x["beta"], x["kappa1"], x["w"], x["a"], x["t0"])
    params[[2]] = c(x["alpha"], x["mu"], x["beta"], x["kappa2"], x["w"], x["a"], x["t0"])
    params[[3]] = c(x["alpha"], x["mu"], x["beta"], x["kappa3"], x["w"], x["a"], x["t0"])
    names(params[[1]]) = names(params[[2]]) = names(params[[3]]) = parameter_names
  }

  # Return negative log-likelihood for function minimization
  if (parameterization == "one_condition") {
    ll_diffusion(x = params,
                 stimuli = data$stimuli[[1]],
                 frame = data$frame[[1]],
                 observations = data$observations[[1]])
  } else {
    ll_diffusion(x = params[[1]],
                 stimuli = data$stimuli[[1]],
                 frame = data$frame[[1]],
                 observations = data$observations[[1]]) +
      ll_diffusion(x = params[[2]],
                   stimuli = data$stimuli[[2]],
                   frame = data$frame[[2]],
                   observations = data$observations[[2]])
  }

}


#' Get parameter names, given a model and parameterization
#'
#' @inheritParams fit_model
#' @export
get_par_names = function(model = "DDM", parameterization = "") {
  # get name of model parameters
  list(DDM = list(one_condition = c("alpha", "mu", "beta", "kappa", "w", "a", "t0"),
                  date_delay_value_sensitivity = c("alpha1", "alpha2", "mu", "beta", "kappa", "w", "a", "t0"),
                  date_delay_time_scaling = c("alpha", "mu", "beta", "kappa1", "kappa2", "w", "a", "t0"),
                  defer_speedup_value_scaling = c("alpha", "mu1", "mu2", "beta", "kappa", "w", "a", "t0"),
                  defer_speedup_time_scaling = c("alpha", "mu", "beta", "kappa1", "kappa2", "kappa3", "w", "a", "t0")))[[model]][[parameterization]]
}

#' Get parameter lower and upper bounds, given a model and parameterization
#'
#' These minimum and maximum values are based on reported values in the literature, see code for details.
#'
#' @inheritParams fit_model
#' @param bound bound for which to obtain values, either "lower" or "upper"
#' @export
get_par_bounds = function(model = "DDM", parameterization = "", bound = "lower") {

  # Lower and upper bounds for each parameter

  lowers = list('alpha' = .01, # source: Dai & Busemeyer, J Exp Psychol Gen, 2014 - p. 1512
                'mu' = 1, # fixed parameter in 'date_delay_value_sensitivity', 'date_delay_time_scaling', and 'defer_speedup_time_scaling' parameterizations
                'mu_loss' = 1, # source: Scholten & Read, J Exp Psychol Learn Mem Cogn, 2013, p. 1197
                'beta' = .01, # source: Dai & Busemeyer, J Exp Psychol Gen, 2014 - p. 1512
                'kappa' = 1, # fixed parameter in 'date_delay_value_sensitivity' and 'defer_speedup_value_scaling' parameterizations
                'kappa_contraction' = 1,
                'kappa_loss' = 1,
                'kappa_gain' = 0,
                'w' = 0.05, # source: Dai & Busemeyer, J Exp Psychol Gen, 2014 - p. 1512
                "a" = 0.5, # source: rtdists R Documentation
                "t0" = 0.05 # source: rtdists R Documentation
                )

  uppers = list('alpha' = 2, # source: Dai & Busemeyer, J Exp Psychol Gen, 2014 - p. 1512
                'mu' = 1, # fixed parameter in 'date_delay_value_sensitivity', 'date_delay_time_scaling', and 'defer_speedup_time_scaling' parameterizations
                'mu_loss' = 3,
                'beta' = 2, # source: Dai & Busemeyer, J Exp Psychol Gen, 2014 - p. 1512
                'kappa' = 1, # fixed parameter in 'date_delay_value_sensitivity' and 'defer_speedup_value_scaling' parameterizations
                'kappa_contraction' = 3, # guess
                'kappa_loss' = 3, # guess
                'kappa_gain' = 1,
                'w' = 0.95, # source: Dai & Busemeyer, J Exp Psychol Gen, 2014 - p. 1512
                "a" = 2, # source: rtdists R Documentation
                "t0" = 3 # source: based on numbers in Table 10 of Dai & Busemeyer, J Exp Psychol Gen, 2014
  )


  # Put in a vector
  l <- unlist(lowers)
  u <- unlist(uppers)

  unname(
    list(DDM = list(one_condition = list(lower = c(l['alpha'], l['mu'], l['beta'], l['kappa'], l['w'], l['a'], l['t0']),
                                         upper = c(u['alpha'], u['mu'], u['beta'], u['kappa'], u['w'], u['a'], u['t0'])
                                         ),
                    date_delay_value_sensitivity = list(lower = c(l['alpha'], l['alpha'], l['mu'], l['beta'], l['kappa'], l['w'], l['a'], l['t0']),
                                                        upper = c(u['alpha'], u['alpha'], u['mu'], u['beta'], u['kappa'], u['w'], u['a'], u['t0'])
                                             ),
                    date_delay_time_scaling = list(lower = c(l['alpha'], l['mu'], l['beta'], l['kappa_contraction'], l['kappa_contraction'], l['w'], l['a'], l['t0']),
                                                   upper = c(u['alpha'], u['mu'], u['beta'], u['kappa_contraction'], u['kappa_contraction'], u['w'], u['a'], u['t0'])
                                            ),
                    defer_speedup_value_scaling = list(lower = c(l['alpha'], l['mu'], l['mu_loss'], l['beta'], l['kappa'], l['w'], l['a'], l['t0']),
                                                       upper = c(u['alpha'], u['mu'], u['mu_loss'], u['beta'], u['kappa'], u['w'], u['a'], u['t0'])
                                               ),
                    defer_speedup_time_scaling = list(lower = c(l['alpha'], l['mu'], l['beta'], l['kappa'], l['kappa_loss'], l['kappa_gain'], l['w'], l['a'], l['t0']),
                                                      upper = c(u['alpha'], u['mu'], u['beta'], u['kappa'], u['kappa_loss'], u['kappa_gain'], u['w'], u['a'], u['t0'])
                                        )
                    )
         )[[model]][[parameterization]][[bound]]
    )
}

#' Intertemporal choice drift diffusion model
#'
#' @param stimuli bla # TODO: use @inher@inheritParams
#' @param parameters ble # TODO: use @inher@inheritParams
#' @export
#' This model is inspired by Dai & Busemeyer, J Exp Psychol Gen, 2014 and Scholten & Read, J Exp Psychol Learn Mem Cogn, 2013
itch_ddm <- function(stimuli, parameters, parameterization = "", frame = "") {

  x = unlist(parameters)

  # Compute drift rate
  v <- compute_drift_rate(parameters = parameters,
                          stimuli = stimuli,
                          parameterization = parameterization,
                          frame = frame)

  # Compute predicted responses
  purrr::map_df(.x = v,
                .f = rtdists::rdiffusion,
                n = 1,
                a = x['a'],
                t0 = x['t0']
                )
}

#' Log-likelihood for drift diffusion model
#'
#' This is based on example in rtdists::ddiffusion package
#'
#' @param pars Vector of parameters
#' @inheritParams rtdists::ddiffusion
#' @export
ll_diffusion <- function(x, stimuli, frame = "", observations) {

  x = unlist(x)

  # Compute drift rate
  v <- compute_drift_rate(x, stimuli, frame)

  # Compute densities
  densities <-
    purrr::pmap_dbl(list(rt = observations$rt,
                         response = observations$response,
                         v = v
                         ),
                    .f = tryCatch(rtdists::ddiffusion, error = function(e) 0),
                    a = x['a'],
                    t0 = x['t0']
                    )

  if (any(densities == 0)) return(1e6)
  return(-sum(log(densities)))
}


#' Make stimulus list (i.e. trials)
#'
#' @param frames character vector of frame names, defaults to c('delay', 'date')
#' @param m_l double vector of monetary amounts of larger option (in Euros), defaults to c(20)
#' @param pct_m_l double vector of monetary amount of smaller option, experessed as percentage of larger option, defaults to c(0.2, 0.5, 0.7, 0.8, 0.88, 0.93, 0.97, 0.99)
#' @param t_s double vector of delay of reward delivery for sooner option (in days), defaults to c(0)
#' @param interval double vector of intervals between sooner and later option (in days), defaults to c(0)
#' @param n_reps number of repetitions of each combination of parameters
#' @return The output is a tibble with columns
make_stimulus_df <- function(frames = c('delay', 'date'),
                             m_l = c(20),
                             pct_m_l = c(0.2, 0.5, 0.7, 0.8, 0.88, 0.93, 0.97, 0.99),
                             t_s = c(0),
                             interval = c(1,2,3,5,7,10,14),
                             n_reps = 1) {

  if (all(c('delay', 'date') %in% frames)) {
    factor_levels <- c('delay','date')
  } else if (all(c('neutral', 'defer', 'speedup') %in% frames)) {
    factor_levels <- c('neutral', 'defer', 'speedup')
  }

  expand.grid(frame = factor(frames, levels = factor_levels),
              pct_m_l = pct_m_l,
              m_l = m_l,
              t_s = t_s,
              interval = interval,
              rep = 1:n_reps
  ) %>%
    dplyr::mutate(m_s = pct_m_l * m_l,
                  t_l = t_s + interval) %>%
    dplyr::select(frame, m_s, t_s, m_l, t_l, dplyr::everything()) %>%
    dplyr::select(-rep) %>%
    tibble::as.tibble()
}

#' Simulate intertemporal choice task performance
#'
#' @param stimuli tibble with trial stimuli
#' @param parameters list with model parameters
#' @param parameterization char vectore specifying model parameterization, defaults to "date_delay_time_scaling"
sim_data <- function(stimuli = make_stimulus_df(),
                     parameters, parameterization = "date_delay_time_scaling") {

  # Assertions
  assertthat::assert_that(parameterization %in% c("one_condition",
                                                  "date_delay_value_sensitivity",
                                                  "defer_speedup_value_scaling",
                                                  "date_delay_time_scaling",
                                                  "defer_speedup_time_scaling"
                                                  ),
                          msg = "Specify parameterization as \n\"one-condition\" (i.e., estimate parameters from a single experimental condition), \n\"date_delay_value_sensitivity\" (i.e., estimate free value function sensitivity parameter across two experimental conditions), \n\"date_delay_time_scaling\" (i.e., estimate free time function sensitivity parameter across two experimental conditions), \n\"defer_speedup_value_scaling\" (i.e., estimate free value function scaling parameter across two experimental conditions), or \n\"defer_speedup_time_scaling\" (i.e., estimate free time function scaling parameter  across two experimental conditions)."
                          )

  # Repeat parameters, if stationary across conditions
  if (parameterization %in% c("one-condition",
                              "date_delay_value_sensitivity",
                              "defer_speedup_value_scaling",
                              "date_delay_time_scaling",
                              "defer_speedup_time_scaling")
  ) {
    parameters <- eq_list_elements(parameters,
                                   n_cond = length(levels(stimuli$frame)))
  }

  # Nested stimuli
  nested_stimuli <-
    stimuli %>%
    dplyr::group_by(frame) %>%
    tidyr::nest(.key = 'stimuli')

  # Nested parameters
  nested_parameters <-
    parameters %>%
    tibble::as.tibble() %>%
    dplyr::mutate(frame = factor(levels(stimuli$frame),
                                 levels = levels(stimuli$frame))) %>%
    dplyr::select(frame, dplyr::everything()) %>%
    dplyr::group_by(frame) %>%
    tidyr::nest(.key = 'parameters')

  # Simulate observations
  nested_stimuli %>%
    dplyr::left_join(nested_parameters, by = "frame") %>%
    dplyr::mutate(observations = purrr::map2(.x = stimuli,
                                             .y = parameters,
                                             .f = itchmodel::itch_ddm)
                  )

}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

eq_list_elements <- function(lst, n_cond) {

  expand_list <- function(x, n) {
    if (length(x) == 1) {
      x = rep(x, length.out = n)
    } else if (length(x) == n) {
      x = x
    }
  }

  if (base::missing(n_cond)) {
    n_cond <- max(unlist(purrr::map(lst, length)))
  }

  purrr::map(lst, expand_list, n_cond)

}


