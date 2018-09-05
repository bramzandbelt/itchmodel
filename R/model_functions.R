# Source some code from John Kruschke for Bayesian logistic regression
# source('/Users/bramzandbelt/surfdrive/projects/BEWITCHING/itchmodel/R/Kruschke_code/Jags-Ydich-XmetMulti-Mlogistic.R')
# source('/Users/bramzandbelt/surfdrive/projects/BEWITCHING/itchmodel/R/Kruschke_code/DBDA2E-utilities.R')

## compute_drift_rate ##########################################################

#' Compute drift rate
#'
#' compute_drift_rate uses approaches described by:
#' - Dai & Busemeyer, J Exp Psychol Gen, 2014
#' - Scholten & Read, J Exp Psychol Learn Mem Cogn, 2013
#'
#' In general, drift rate is computed according to Dai & Busemeyer (2014):
#' v = x['w'] * (um_l - um_s) - (1 - x['w']) * (pt_l - pt_s)
#'
#' In case of the defer-speedup effect under outcome framing, we follow Scholten & Read (2013). See their p. 1197.
#'
#' @param parameters parameters
#' @param stimuli stimuli
#' @param frame the way in which the choice was framed: "defer", "speedup", "date", "delay
#' @inheritParams itchmodel::fit_model
#' @export
compute_drift_rate <- function(parameters, stimuli, parameterization = "", frame = "") {

  # 1. Unlist parameters =======================================================

  x <- unlist(parameters)

  # 2. Compute drift rate ======================================================
  # Drift rate, v, is computed as:
  #
  # v = w * du - (1 - w) * dp, where
  #
  # w is a weighting parameter
  # du is the difference in utility between the large and small option
  # dp is the difference in weighted/perceived time between the late and soon option

  unname(x['w'] *
    compute_transformation_diffs(parameters = parameters,
                                 stimuli = stimuli,
                                 parameterization = parameterization,
                                 frame = frame,
                                 variable = 'du') -
    (1 - x['w']) *
    compute_transformation_diffs(parameters = parameters,
                                 stimuli = stimuli,
                                 parameterization = parameterization,
                                 frame = frame,
                                 variable = 'dp'))
}

#'
#'
#' @export
compute_drift_rate_transformation_diffs <- function(parameters, stimuli, parameterization = "", frame = "") {

  x <- unlist(parameters)

  tibble::tibble(variable = character(),
                   value = double(),
                   weighted = logical()) %>%
    tibble::add_row(variable = 'du',
                    value = compute_transformation_diffs(parameters = parameters,
                                                         stimuli = stimuli,
                                                         parameterization = parameterization,
                                                         frame = frame,
                                                         variable = 'du'),
                    weighted = FALSE) %>%
    tibble::add_row(variable = 'dp',
                    value = compute_transformation_diffs(parameters = parameters,
                                                         stimuli = stimuli,
                                                         parameterization = parameterization,
                                                         frame = frame,
                                                         variable = 'dp'),
                    weighted = FALSE) %>%
    tibble::add_row(variable = 'du',
                    value = unname(x['w']) *
                      compute_transformation_diffs(parameters = parameters,
                                                   stimuli = stimuli,
                                                   parameterization = parameterization,
                                                   frame = frame,
                                                   variable = 'du'),
                    weighted = TRUE) %>%
    tibble::add_row(variable = 'dp',
                    value = (1 - unname(x['w'])) *
                      compute_transformation_diffs(parameters = parameters,
                                                   stimuli = stimuli,
                                                   parameterization = parameterization,
                                                   frame = frame,
                                                   variable = 'dp'),
                    weighted = TRUE) %>%
    dplyr::mutate(variable = factor(variable, levels = c('du', 'dp')),
                  weighted = factor(weighted, levels = c('FALSE', 'TRUE'))
                  )


}


## compute_transformation_diffs ################################################
#' Computes the differences in utilities (du) and weighted times (dp) for the small-but-soon and large-but-later options, given a set of parameters and stimuli.
compute_transformation_diffs <- function(parameters, stimuli, parameterization = "", frame = "", variable = 'du') {

  if (variable == 'du') {
    # Differences in utility ===================================================

    # Utility of the large option
    compute_transformation(q = stimuli$m_l,
                           parameters = parameters,
                           variable = 'u_ll',
                           frame = frame) -

      # Utility of the small option
      compute_transformation(q = stimuli$m_s,
                             parameters = parameters,
                             variable = 'u_ss',
                             frame = frame)


  } else if (variable == 'dp') {
    # Differences in weighted/perceived time ===================================

    # Weighted/perceived time of the late option
    compute_transformation(q = stimuli$t_l,
                           parameters = parameters,
                           variable = 'p_ll',
                           frame = frame) -

      # Weighted/perceived time of the soon option
      compute_transformation(q = stimuli$t_s,
                             parameters = parameters,
                             variable = 'p_ss',
                             frame = frame)

  } else {
    NA
  }
}


## compute_transformation ######################################################
#' Given a set of parameters, compute the money-to-utility and clock-time-to-perceived-time transformation functions
#'
#' @param parameters Named list of parameters, including alpha, mu, beta, and kappa
#' @param x optional double vector of values to be transformed (i.e. money/time)
#' @export
compute_transformation <- function(q, parameters, variable, frame = "") {

  # 1. Unlist parameters =======================================================

  x <- unlist(parameters)

  # 2. Transform money/utility into utility/weighted time ======================

  if (variable == 'u') {

    # 2.1. Utility (used for date and delay frames) ----------------------------

    unname(convert(q, type = 'power', param = c(x['alpha'], x['mu'])))

  } else if (variable == 'u_ss') {

    # 2.2. Utility of small-but-soon option (neutral, defer, speedup frames) ---

    if (frame %in% c('neutral','speedup')) {

      unname(convert(q, type = 'power', param = c(x['alpha'], 1)))

    } else if (frame == 'defer') {

      unname(convert(q, type = 'power', param = c(x['alpha'], x['mu'])))

    } else {

      unname(convert(q, type = 'power', param = c(x['alpha'], x['mu'])))

    }

  } else if (variable == 'u_ll') {

    # 2.3. Utility of large-but-later option (neutral, defer, speedup frames) --

    if (frame %in% c('neutral','defer')) {

      unname(convert(q, type = 'power', param = c(x['alpha'], 1)))

    } else if (frame == 'speedup') {

      unname(convert(q, type = 'power', param = c(x['alpha'], x['mu'])))

    } else {

      unname(convert(q, type = 'power', param = c(x['alpha'], x['mu'])))
    }
  } else if (variable %in% c('p', 'p_ss', 'p_ll')) {

    # 2.3. Weighted time (any frame) -------------------------------------------

    unname(convert(q, type = 'power', param = c(x['beta'], x['kappa'])))

  } else {

    NA

  }
}


## convert #####################################################################
#' Convert a quantity into another according to a transformation function. Transformation functions are 'power' (based on Dai & Busemeyer, J Exp Gen Psychol, 2014) and 'log' (based on Scholten & Read, J Exp Psychol Learn Mem Cogn, 2013).
#'
#' @param q quantity (either money in Euros or time in days)
#' @param type transformation function
#' @param params transformation function parameters
#' @export
convert <- function(q, type = 'power', param) {

  # 1. Assertions ==============================================================
  assertthat::assert_that(all(q >= 0), msg = 'Make sure that money and time should be ≥ 0')
  assertthat::assert_that(is.character(type))
  assertthat::assert_that(is.vector(param))
  assertthat::assert_that(length(param) == 2)
  assertthat::assert_that(is.numeric(param))

  # 2. Transform data ==========================================================
  switch(type,
         power = param[2] * q^param[1],
         log = param[2] * (1 / param[1] * log(1 + param[1] * q))
  )
}


## db_bin_choice_prob ##########################################################
#' Dai & Busemeyer equation for binary choice probability of a specific option
#'
#' Note that this gives same results as rtdists::rdiffusion with identical parameters (but note that the latter has bound separation parameter a, which equals 2 * threshold in Dai & Busemeyer equation)
#'
#' The following should get the same output:
#' a <- runif(n = 1, min = 0.5, max = 2) # threshold separation
#' v <- runif(n = 1, min = -5, max = 5) # drift rate
#' s <- 1 # diffusion constant
#' t0 <- runif(n = 1, min = 0, max = 2) # nondecision time
#' itchmodel::db_bin_choice_prob(d = v, s = s, a = a, z = 0)
#'
#' rtdists::pdiffusion(rt = Inf, response = "upper", a = a, v = v, t0 = t0, s = s, z = 0.5*a)
#'
#' @export
db_bin_choice_prob <- function(d, s, a, z) {

  theta <- a / 2

  if (any(dplyr::near(d,0, tol = 1e-9))) {
    d <- d + 1e-9
  }

  # Dividing infinite numbers results in NaNs; make negative drift rates smaller to prevent this
  d <- ifelse((-2 * d * (theta + z) / s^2 > log(.Machine$double.xmax) |
            -4 * d * theta / s^2 > log(.Machine$double.xmax)),
         max(c((log(.Machine$double.xmax) * s^2) / (-2 * (theta + z)),
               (log(.Machine$double.xmax) * s^2) / (-4 * theta))),
         d)

  # if (-2 * d * (theta + z) / s^2 > log(.Machine$double.xmax) |
  #     -4 * d * theta / s^2 > log(.Machine$double.xmax)) {
  #   d <-
  #     max(c((log(.Machine$double.xmax) * s^2) / (-2 * (theta + z)),
  #         (log(.Machine$double.xmax) * s^2) / (-4 * theta)))
  # }

  numerator <- (1 - exp(-2 * d * (theta + z) / s^2))
  denominator <- (1 - exp(-4 * d * theta / s^2))

  numerator / denominator

}

## dft_cp ######################################################################
#' Compute choice probability density, according to decision field theory
#'
#' Code is based on the function CP.m by Junyi Dai
#'
#' @export
#'
dft_cp <- function(d, s, theta, z, response) {

  theta <- max(c(theta, 1e-4))
  if (response == "lower") {
    d <- -d
    z <- -z
  }

  if (-4 * d * theta / s^2 > 500) {
    cp <- .0001
  } else {
    if (d == 0) {
      d = 1e-100
    }

    cp <- expm1(-2 * d * (theta + z) / s^2) / expm1(-4 * d * theta / s^2)
    cp <- max(min(cp, .9999), .0001)
  }

}

## dft_dpd #####################################################################
#' Compute defective probability density of a specific response time , according
#' to to decision field theory
#'
#' Code is based on the function DPD.m by Junyi Dai
#'
#' @export
#'
dft_dpd <- function(d, s, theta, z, response, rtd) {

  if (response == "lower") {
    d <- -d
    z <- -z
  }

  tolerance <- 1e-200
  j <- 1
  dpd <- 0

  step_size <-
    pi * (2 * theta / s)^(-2) * j *
    exp(d * (theta - z) / s^2 -
          ((pi * j * s / (2 * theta))^2 + (d / s)^2) * rtd / 2)
  while (step_size > tolerance) {
    dpd <- dpd + step_size * sin(pi * (theta - z) * j / (2 * theta))
    j <- j + 1
    step_size <-
      pi * (2 * theta / s)^(-2) * j *
      exp(d * (theta - z) / s^2 -
            ((pi * j * s / (2 * theta))^2 + (d / s)^2) * rtd / 2);

  }


  if (dpd > 0) {
    pd <- dpd
  } else {
    pd <- 1e-200
  }

  return(pd)

}

## eq_list_elements ############################################################
#' Ensure that list elemens are equal across conditions
#'
#' Parameter values not varying between conditions are repeated
#'
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
## fit_model ###################################################################
#' Function for fitting intertemporal choice diffusion model
#'
#' @param data Tibble with nested data
#' @param model Name of the model. Currently, three models are implemented: decision field theory - choices only (DFT_C), decision field theory - choices and response times (DFT_CRT), and the drift diffusion model (DDM)
#' @param parameterization Name of the parameterization. Currently, there are nine possibilities: (1) date_delay_time_scaling; (2) date_delay_time_scaling_t0; (3) date_delay_value_scaling; (4) date_delay_value_scaling_t0; (5) defer_speedup_value_scaling; (6) defer_speedup_value_scaling_t0; (7) defer_speedup_time_scaling; (8) defer_speedup_time_scaling_t0, and (9) one_condition. The parameterizations in which t0 varies between conditions is only available for DFT_CRT and DDM.
#' @param control A list of control parameters for the Differential Evolution algorithm
fit_model <- function(data,
                      model = "",
                      parameterization = "",
                      lowers = get_par_bounds(model = model,
                                              parameterization = parameterization,
                                              bound = 'lower'),
                      uppers = get_par_bounds(model = model,
                                              parameterization = parameterization,
                                              bound = 'upper'),
                      control = list(itermax = 250, reltol = 1e-5)
                      ) {

  # 1. Assert that inputs meet certain conditions ==============================

  assertthat::assert_that(model %in% c("DDM", "DFT_C", "DFT_CRT"),
                          msg = "model is ill-specified. Currently, only the following models are supported: \n'DDM', 'DFT_C', and 'DFT_CRT'")

  assertthat::assert_that(parameterization %in% c('one_condition',
                                                  'date_delay_time_scaling',
                                                  'date_delay_time_scaling_t0',
                                                  'date_delay_value_scaling',
                                                  'date_delay_value_scaling_t0',
                                                  'defer_speedup_time_scaling',
                                                  'defer_speedup_time_scaling_t0',
                                                  'defer_speedup_value_scaling',
                                                  'defer_speedup_value_scaling_t0'),
                          msg = "parameterization is ill-specified. It should be any of the following: \n'date_delay_time_scaling', \n'date_delay_time_scaling_t0', \n'date_delay_value_scaling', \n'date_delay_value_scaling_t0', \n'defer_speedup_time_scaling', \n'defer_speedup_time_scaling_t0', \n'defer_speedup_value_scaling', \n'defer_speedup_value_scaling_t0', or \n'one_condition', .")

  assertthat::assert_that(is.double(lowers), msg = "lowers should be a double-precision vector")
  assertthat::assert_that(is.double(uppers), msg = "uppers should be a double-precision vector")

  # 2. Use Differential Evolution to optimize model parameters =================
  optim_out <-
    DEoptim::DEoptim(fn = itchmodel::get_log_likelihood, # optimization function
                     lower = lowers,
                     upper = uppers,
                     data = data,
                     model = model,
                     parameterization = parameterization,
                     control = control
                     )

  # Nonlinear Constrained and Unconstrained Optimization via Differential Evolution
  # optim_out <-
  # DEoptimR::JDEoptim(fn = itchmodel::get_log_likelihood, # optimization function
  #                    lower = lowers,
  #                    upper = uppers,
  #                    constr = get_nonlinear_constraints(data = data,
  #                                                       model = model,
  #                                                       parameterization = parameterization),
  #                    data = data,
  #                    model = model,
  #                    parameterization = parameterization,
  #                    control = control
  # )

  # Add model and parameterization to output
  attributes(optim_out)$model = model
  attributes(optim_out)$parameterisation = parameterization

  # 3. Return output ===========================================================
  optim_out

}

## get_default_parameter_values ################################################
#' Returns default values for model parameters
#'
#' @export
get_default_parameter_values <- function() {
  list('alpha' = 0.85, # Sensitivity of the value function
       'mu' = 1, # Scaling of the value function
       'beta' = 0.70, # Sensitivity of the time function
       'kappa' = 1, # Scaling of the time function
       'w' = 0.5, # Attentional weight
       "a" = 1, # Bound separation (threshold equals a * 2)
       "t0" = 0.3 # Non-decision time,
  )
}


## get_fit_stats ###############################################################
#' Returns model fit statistics
#'
#' @inheritParams fit_model
#' @param optim_output output from DEoptim::DEoptim
#' @param n_data_points number of data points (trials)
#' DEoptimR minimizes negative log likelihood. get_fit_stats computes log likelihood (LL), Akaike Information Criterion (AIC), and Bayesian Information Criterion (BIC) based on that.
#'
#' @export
get_fit_stats <- function(optim_output, model = "", parameterization = "", n_data_points) {

  # Number of free parameters
  n_free_param <- get_n_free_param(model = model,
                                   parameterization = parameterization)

  # Log-likelihood
  LL <- -optim_output$value

  # Return fit statistics
  tibble::tibble(model = model,
                 parameterization = parameterization,
                 n_iter = optim_output$iter,
                 converged = ifelse(optim_out_gen_model$convergence == 0, "TRUE", "FALSE"),
                 bestval = optim_output$value,
                 LL = LL,
                 AIC = -2 * LL + 2 * n_free_param,
                 BIC = -2 * LL + log(n_data_points) * n_free_param
                 )

}

## get_frames ##################################################################
#' Returns the names of the framing conditions, given a parameterization
#'
#' @export
get_frames <- function(parameterization) {

  # 1. Assert that inputs meet expectations ====================================
  assertthat::assert_that(parameterization %in% c("one_condition",
                                                  "date_delay_value_scaling",
                                                  "date_delay_value_scaling_t0",
                                                  "defer_speedup_value_scaling",
                                                  "defer_speedup_value_scaling_t0",
                                                  "date_delay_time_scaling",
                                                  "date_delay_time_scaling_t0",
                                                  "defer_speedup_time_scaling",
                                                  "defer_speedup_time_scaling_t0"
  ),
  msg = "Specify parameterization as \n\"one-condition\" (i.e., estimate parameters from a single experimental condition), \n\"date_delay_value_scaling\" (i.e., estimate free value function scaling parameter across two experimental conditions), \n\"date_delay_value_scaling_t0\" (i.e., estimate free value function scaling parameter and t0 across two experimental conditions), \n\"date_delay_time_scaling\" (i.e., estimate free time function sensitivity parameter across two experimental conditions), \n\"date_delay_time_scaling_t0\" (i.e., estimate free time function sensitivity parameter and t0 across two experimental conditions), \n\"defer_speedup_value_scaling\" (i.e., estimate free value function scaling parameter across two experimental conditions), \n\"defer_speedup_value_scaling_t0\" (i.e., estimate free value function scaling parameter and t0 across two experimental conditions), \n\"defer_speedup_time_scaling\" (i.e., estimate free time function scaling parameter  across two experimental conditions), or \n\"defer_speedup_time_scaling_t0\" (i.e., estimate free time function scaling parameter and t0 across two experimental conditions)."
  )

  # 2. Return frames ===========================================================
  if (startsWith(parameterization, "one_condition")) {
    'neutral'
  } else if (startsWith(parameterization, "date_delay")) {
    c('delay', 'date')
  } else if (startsWith(parameterization, "defer_speedup")) {
    c('neutral', 'defer', 'speedup')
  } else {
    NA
  }
}

## get_log_likelihood  #########################################################
#' Get the log-likelihood of the parameters, given the data
#'
#' This function is a wrapper around ll_diffusion.
#'
#' @param x parameters
#' @inheritParams fit_model
#' @export
get_log_likelihood = function(x, data, model = "DFT_C", parameterization = "") {

  # 1. Get parameter values ==================================================
  params <- get_par_values(x, model = model, parameterization = parameterization)

  # 2. Return negative log-likelihood for function minimization ================
  ll <- 0

  if (model == "DFT_C") {
    for (i_cond in 1:length(params)) {
      ll <-
        ll +
        ll_dft(x = params[[i_cond]],
               stimuli = data$stimuli[[i_cond]],
               frame = data$frame[[i_cond]],
               observations = data$observations[[i_cond]],
               rt = FALSE)
    }
  } else if (model == "DFT_CRT") {
    for (i_cond in 1:length(params)) {
      ll <-
        ll +
        ll_dft(x = params[[i_cond]],
               stimuli = data$stimuli[[i_cond]],
               frame = data$frame[[i_cond]],
               observations = data$observations[[i_cond]],
               rt = TRUE)
    }
  } else if (model == "DDM") {
    for (i_cond in 1:length(params)) {
      ll <-
        ll +
        ll_diffusion(x = params[[i_cond]],
                     stimuli = data$stimuli[[i_cond]],
                     frame = data$frame[[i_cond]],
                     observations = data$observations[[i_cond]])
    }
  }



  ll

}




## get_m_ss_estimates ##########################################################
#' Based on response data from an indifference point procedure, estimate values for m_ss that will more or less in P(choose LL) equal to p
#'
#' @param data response data
#' @param p vector of desired response probabilities
get_m_ss_estimates <- function(data, p) {

  # Fit Bayesian logistic regression to data

  nested_data <-
    data %>%
    dplyr::select(m_ss, t_ll, choice) %>%
    dplyr::mutate(choice = as.integer(choice)) %>%
    # Nest based on t_ll
    tidyr::nest(., .key = 'obs', c(m_ss, choice))  %>%
    dplyr::mutate(obs = purrr::map(.x = .$obs,
                                   .f = as.data.frame
                                   )
                  )

  # I could not get this to work quickly with purrr
  for (i_row in 1:nrow(nested_data)) {

    nested_data$mcmc_coda[[i_row]] = genMCMC(data = nested_data$obs[[i_row]],
                                             xName = c("m_ss"),
                                             yName = "choice",
                                             numSavedSteps = 10000,
                                             saveName = NULL
                                             )
    nested_data$params[[i_row]] = smryMCMC(nested_data$mcmc_coda[[i_row]])

  }

  # Convert factor levels to numeric (for calculation purposes)
  nested_data$t_ll <- as.numeric(levels(nested_data$t_ll)[nested_data$t_ll])

  for (i_row in 1:nrow(nested_data)) {
    nested_data$m_ss[[i_row]] <-
      # expand.grid(p = p,
      #             t_ll = nested_data$t_ll[[i_row]]) %>%
      # tibble::as.tibble() %>%
      (log(1/p-1) +
         nested_data$params[[i_row]]['beta0','Median']) /
      -nested_data$params[[i_row]]['beta','Median']

  }
  nested_data$p = rep(list(p), nrow(nested_data))

  nested_data %>% dplyr::select(t_ll, p, m_ss) %>% tidyr::unnest()
}

## get_par_names ###############################################################
#' Get parameter names, given a model and parameterization
#'
#' @inheritParams fit_model
#' @export
get_par_names = function(model = "DFT_C", parameterization = "") {

  # 1. Get model parameter names ===============================================
  list(DDM =
         list(
           # 1.1. One condition ------------------------------------------------
           one_condition = c("alpha", "mu", "beta", "kappa", "w", "a", "t0"),

           # 1.2.1. Date/delay effect - changes in time scaling (kappa) ----------
           # - delay framing: kappa = 1
           # - date framing: kappa = kappa_loss < 1

           date_delay_time_scaling = c("alpha", "mu", "beta", "kappa1", "kappa2", "w", "a", "t0"),

           # 1.2.2. Date/delay effect - changes in time scaling (kappa) & t0----
           # - delay framing: kappa = 1, t0 = t01
           # - date framing: kappa = kappa_loss < 1, , t0 = t02

           date_delay_time_scaling_t0 = c("alpha", "mu", "beta", "kappa1", "kappa2", "w", "a", "t01", "t02"),

           # 1.3.1. Date/delay effect - changes in value scaling (mu) ----------
           # - delay framing: mu = 1
           # - date framing: mu = mu_gain > 1

           date_delay_value_scaling = c("alpha", "mu1", "mu2", "beta", "kappa", "w", "a", "t0"),

           # 1.3.2. Date/delay effect - changes in value scaling (mu) & t0 -----
           # - delay framing: mu = 1, t0 = t01
           # - date framing: mu = mu_gain > 1, , t0 = t02

           date_delay_value_scaling_t0 = c("alpha", "mu1", "mu2", "beta", "kappa", "w", "a", "t01", "t02"),

           # 1.4.1. Defer/speedup effect - changes in time scaling (kappa) -----
           # - neutral framing: kappa = 1
           # - defer framing: kappa > 1 (over-responsive to deferrals)
           # - speedup framing: kappa < 1 (under-responsive to speedups)

           defer_speedup_time_scaling = c("alpha", "mu", "beta", "kappa1", "kappa2", "kappa3", "w", "a", "t0"),

           # 1.4.2. Defer/speedup effect - changes in time scaling (kappa) & t0
           # - neutral framing: kappa = 1, t0 = t01
           # - defer framing: kappa > 1 (over-responsive to deferrals), t0 = t02
           # - speedup framing: kappa < 1 (under-responsive to speedups), t0 = t03

           defer_speedup_time_scaling_t0 = c("alpha", "mu", "beta", "kappa1", "kappa2", "kappa3", "w", "a", "t01", "t02", "t03"),

           # 1.5.1. Defer/speedup effect - changes in value scaling (mu) -------
           # - neutral framing:
           # - defer framing:
           # - speedup framing:

           defer_speedup_value_scaling = c("alpha", "mu1", "mu2", "beta", "kappa", "w", "a", "t0"),
           # 1.5.2. Defer/speedup effect - changes in value scaling (mu) & t0 --
           # - neutral framing:
           # - defer framing:
           # - speedup framing:

           defer_speedup_value_scaling_t0 = c("alpha", "mu1", "mu2", "beta", "kappa", "w", "a", "t01", "t02", "t03")
         ),
       DFT_C =
         list(
           # 1.1. One condition ------------------------------------------------
           one_condition = c("alpha", "mu", "beta", "kappa", "w", "theta_star"),

           # 1.2.1. Date/delay effect - changes in time scaling (kappa) ----------
           # - delay framing: kappa = 1
           # - date framing: kappa = kappa_loss < 1

           date_delay_time_scaling = c("alpha", "mu", "beta", "kappa1", "kappa2", "w", "theta_star"),

           # 1.3.1. Date/delay effect - changes in value scaling (mu) ----------
           # - delay framing: mu = 1
           # - date framing: mu = mu_gain > 1

           date_delay_value_scaling = c("alpha", "mu1", "mu2", "beta", "kappa", "w", "theta_star"),

           # 1.4.1. Defer/speedup effect - changes in time scaling (kappa) -----
           # - neutral framing: kappa = 1
           # - defer framing: kappa > 1 (over-responsive to deferrals)
           # - speedup framing: kappa < 1 (under-responsive to speedups)

           defer_speedup_time_scaling = c("alpha", "mu", "beta", "kappa1", "kappa2", "kappa3", "w", "theta_star"),

           # 1.5.1. Defer/speedup effect - changes in value scaling (mu) -------
           # - neutral framing:
           # - defer framing:
           # - speedup framing:

           defer_speedup_value_scaling = c("alpha", "mu1", "mu2", "beta", "kappa", "w", "theta_star")

         ),
       DFT_CRT =
         list(
           # 1.1. One condition ------------------------------------------------
           one_condition = c("alpha", "mu", "beta", "kappa", "w", "theta_star"),

           # 1.2.1. Date/delay effect - changes in time scaling (kappa) ----------
           # - delay framing: kappa = 1
           # - date framing: kappa = kappa_loss < 1

           date_delay_time_scaling = c("alpha", "mu", "beta", "kappa1", "kappa2", "w", "theta_star"),

           # 1.3.1. Date/delay effect - changes in value scaling (mu) ----------
           # - delay framing: mu = 1
           # - date framing: mu = mu_gain > 1

           date_delay_value_scaling = c("alpha", "mu1", "mu2", "beta", "kappa", "w", "theta_star"),

           # 1.4.1. Defer/speedup effect - changes in time scaling (kappa) -----
           # - neutral framing: kappa = 1
           # - defer framing: kappa > 1 (over-responsive to deferrals)
           # - speedup framing: kappa < 1 (under-responsive to speedups)

           defer_speedup_time_scaling = c("alpha", "mu", "beta", "kappa1", "kappa2", "kappa3", "w", "theta_star"),

           # 1.5.1. Defer/speedup effect - changes in value scaling (mu) -------
           # - neutral framing:
           # - defer framing:
           # - speedup framing:

           defer_speedup_value_scaling = c("alpha", "mu1", "mu2", "beta", "kappa", "w", "theta_star")

         )
  )[[model]][[parameterization]]

}

## get_n_free_param ##############################################################
#' Get number of free parameters, given parameterization
#'
#' @inheritParams get_par_bounds
#' @export
get_n_free_param = function(model = "DFT_C", parameterization = "") {

  # 1. Get lower and upper bounds for all parameters ===========================

  l <- get_par_bounds(model = model,
                      parameterization = parameterization,
                      bound = 'lower')
  u <- get_par_bounds(model = model,
                      parameterization = parameterization,
                      bound = 'upper')

  # 2. Determine number of free parameters based on bounds =====================
  sum(!(l == u))
}


## get_nonlinear_constraints ###################################################
#' Get nonlinear constraints
#' @param x vector of parameters
#' @param model
#' @param parameterizationn model parameterization
#' @export
get_nonlinear_constraints <- function(x, data, model = "DFT_C", parameterization = "") {

  # 1. Get parameter values ====================================================
  x_named <- get_par_values(x, model = model, parameterization = parameterization)

  # 2. Get frames ==============================================================
  frames <- as.character(data$frame)

  # 4. Formulate linear inequalities ===========================================

  lineq <- double()

  # 4.1. The costs of waiting cannot be greater than the benefits associated with the large reward:
  # (1 - w) * (p_ll - p_ss) - w * u_ll <= 0
  for (i_frame in length(frames)) {

    frame <- frames[i_frame]
    params <- unlist(x_named[i_frame])
    stimuli <- data$stimuli[data$frame == frame][[1]]

    lineq <- c(lineq,
               (1 - params['w']) *
                 (compute_transformation(q = stimuli$t_l,
                                         parameters = params,
                                         variable = 'p_ll',
                                         frame = frame) -
                    compute_transformation(q = stimuli$t_s,
                                           parameters = params,
                                           variable = 'p_ss',
                                           frame = frame)) -
                 params['w'] *
                 compute_transformation(q = stimuli$m_l,
                                        parameters = params,
                                        variable = 'u_ll',
                                        frame = frame)
               )



  }

  # Output
  unname(lineq)

}

## get_par_bounds ##############################################################
#' Get parameter lower and upper bounds, given a model and parameterization
#'
#' These minimum and maximum values are based on reported values in the literature or best guesses, see code for details.
#'
#' @inheritParams fit_model
#' @param bound bound for which to obtain values, either "lower" or "upper"
#' @export
get_par_bounds = function(model = "DFT_C", parameterization = "", bound = "lower") {

  # 1. Define lower and upper bounds for all parameters ========================
  # Values are based on the following sources:
  # DB_JEPG_2014 - Dai & Busemeyer, J Exp Psychol Gen, 2014 - p. 1512
  # SR_JEPLMC_2013 - Scholten & Read, J Exp Psychol Learn Mem Cogn, 2013, p. 1197
  # rtdists - documentation accompanying the R package rtdists

  lowers = list('alpha' = .01, # DB_JEPG_2014
                'mu' = 1, # SR_JEPLMC_2013
                'mu_gain' = 1,
                'mu_loss' = 1, # SR_JEPLMC_2013
                'beta' = .01, # DB_JEPG_2014
                'kappa' = 1, # SR_JEPLMC_2013
                'kappa_gain' = 0,
                'kappa_loss' = 1,
                'w' = 0.05, # DB_JEPG_2014
                "a" = 0.1, # Adjusted by BBZ; rtdists: 0.5
                "t0" = 0.05, # rtdists; N.B. lowers identical across different parameterizations
                "theta_star" = 0.01 # DB_JEPG_2014
                )

  uppers = list('alpha' = 2, # DB_JEPG_2014
                'mu' = 1, #
                'mu_gain' = 3,
                'mu_loss' = 3, # SR_JEPLMC_2013
                'beta' = 2, # DB_JEPG_2014
                'kappa' = 1, # SR_JEPLMC_2013
                'kappa_gain' = 1,
                'kappa_loss' = 10, # guess
                'w' = 0.95, # DB_JEPG_2014
                "a" = 10, # Adjusted by BBZ; rtdists: 2
                "t0" = 3, # DB_JEPG_2014 (data in Table 10); N.B. uppers identical across different parameterizations
                "theta_star" = 100 # DB_JEPG_2014
  )

  # Put in a vector
  l <- unlist(lowers)
  u <- unlist(uppers)


  # 2. Return correct values, based on model, parameterization, and bound ======
  unname(
    list(DDM =
           list(one_condition =
                  list(lower = c(l['alpha'], l['mu'], l['beta'], l['kappa'], l['w'], l['a'], l['t0']),
                       upper = c(u['alpha'], u['mu'], u['beta'], u['kappa'], u['w'], u['a'], u['t0'])
                       ),

                # 1. Date-delay effect =========================================

                # 1.1.1. Changes in time scaling (kappa) -------------------------
                # - delay framing: kappa = 1
                # - date framing: kappa = kappa_loss < 1

                date_delay_time_scaling =
                  list(lower = c(l['alpha'], l['mu'], l['beta'], l['kappa'], l['kappa_gain'], l['w'], l['a'], l['t0']),
                       upper = c(u['alpha'], u['mu'], u['beta'], u['kappa'], u['kappa_gain'], u['w'], u['a'], u['t0'])
                  ),

                # 1.1.2. Changes in time scaling (kappa) & t0 ------------------
                # - delay framing: kappa = 1, t0 = t01
                # - date framing: kappa = kappa_loss < 1, t0 = t02

                date_delay_time_scaling_t0 =
                  list(lower = c(l['alpha'], l['mu'], l['beta'], l['kappa'],
                                 l['kappa_gain'], l['w'], l['a'], l['t0'], l['t0']),
                       upper = c(u['alpha'], u['mu'], u['beta'], u['kappa'],
                                 u['kappa_gain'], u['w'], u['a'], u['t0'], u['t0'])
                  ),

                # 1.2.1. Changes in value scaling (mu) ---------------------------
                # - delay framing: mu = 1
                # - date framing: mu = mu_gain > 1

                date_delay_value_scaling =
                  list(lower = c(l['alpha'], l['mu'], l['mu_gain'], l['beta'],
                                 l['kappa'], l['w'], l['a'], l['t0']),
                       upper = c(u['alpha'], u['mu'], u['mu_gain'], u['beta'],
                                 u['kappa'], u['w'], u['a'], u['t0'])
                  ),

                # 1.2.2. Changes in value scaling (mu) ---------------------------
                # - delay framing: mu = 1, t0 = t01
                # - date framing: mu = mu_gain > 1, t0 = t02

                date_delay_value_scaling_t0 =
                  list(lower = c(l['alpha'], l['mu'], l['mu_gain'], l['beta'],
                                 l['kappa'], l['w'], l['a'], l['t0'], l['t0']),
                       upper = c(u['alpha'], u['mu'], u['mu_gain'], u['beta'],
                                 u['kappa'], u['w'], u['a'], u['t0'], u['t0'])
                  ),

                # 2. Defer-speedup effect ======================================

                # 2.1.1. Changes in time scaling (kappa) -----------------------
                # - neutral framing: kappa = 1
                # - defer framing: kappa > 1 (over-responsive to deferrals)
                # - speedup framing: kappa < 1 (under-responsive to speedups)

                defer_speedup_time_scaling =
                  list(lower = c(l['alpha'], l['mu'], l['beta'], l['kappa'],
                                 l['kappa_loss'], l['kappa_gain'], l['w'], l['a'], l['t0']),
                       upper = c(u['alpha'], u['mu'], u['beta'], u['kappa'],
                                 u['kappa_loss'], u['kappa_gain'], u['w'], u['a'], u['t0'])
                  ),

                # 2.1.2. Changes in time scaling (kappa) -----------------------
                # - neutral framing: kappa = 1, t0 = t01
                # - defer framing: kappa > 1 (over-responsive to deferrals), t0 = t02
                # - speedup framing: kappa < 1 (under-responsive to speedups), t0 = t03

                defer_speedup_time_scaling_t0 =
                  list(lower = c(l['alpha'], l['mu'], l['beta'], l['kappa'],
                                 l['kappa_loss'], l['kappa_gain'], l['w'],
                                 l['a'], l['t0'], l['t0'], l['t0']),
                       upper = c(u['alpha'], u['mu'], u['beta'], u['kappa'],
                                 u['kappa_loss'], u['kappa_gain'], u['w'],
                                 u['a'], u['t0'], u['t0'], u['t0'])
                  ),

                # 2.2.1. Changes in value scaling (mu) ---------------------------
                # - neutral framing:
                # - defer framing:
                # - speedup framing:

                defer_speedup_value_scaling =
                  list(lower = c(l['alpha'], l['mu'], l['mu_loss'], l['beta'],
                                 l['kappa'], l['w'], l['a'], l['t0']),
                       upper = c(u['alpha'], u['mu'], u['mu_loss'], u['beta'],
                                 u['kappa'], u['w'], u['a'], u['t0'])
                  ),

                # 2.2.2. Changes in value scaling (mu) ---------------------------
                # - neutral framing:
                # - defer framing:
                # - speedup framing:

                defer_speedup_value_scaling_t0 =
                  list(lower = c(l['alpha'], l['mu'], l['mu_loss'], l['beta'],
                                 l['kappa'], l['w'], l['a'], l['t0'], l['t0'], l['t0']),
                       upper = c(u['alpha'], u['mu'], u['mu_loss'], u['beta'],
                                 u['kappa'], u['w'], u['a'], u['t0'], u['t0'], u['t0'])
                  )


                ),
         DFT_C = list(one_condition =
                        list(lower = c(l['alpha'], l['mu'], l['beta'], l['kappa'],
                                       l['w'], l['theta_star']),
                             upper = c(u['alpha'], u['mu'], u['beta'], u['kappa'],
                                       u['w'], u['theta_star'])
                        ),

                      # 1. Date-delay effect =========================================

                      # 1.1.1. Changes in time scaling (kappa) -------------------------
                      # - delay framing: kappa = 1
                      # - date framing: kappa = kappa_loss < 1

                      date_delay_time_scaling =
                        list(lower = c(l['alpha'], l['mu'], l['beta'], l['kappa'],
                                       l['kappa_gain'], l['w'], l['theta_star']),
                             upper = c(u['alpha'], u['mu'], u['beta'], u['kappa'],
                                       u['kappa_gain'], u['w'], u['theta_star'])
                        ),

                      # 1.2.1. Changes in value scaling (mu) ---------------------------
                      # - delay framing: mu = 1
                      # - date framing: mu = mu_gain > 1

                      date_delay_value_scaling =
                        list(lower = c(l['alpha'], l['mu'], l['mu_gain'], l['beta'],
                                       l['kappa'], l['w'], l['theta_star']),
                             upper = c(u['alpha'], u['mu'], u['mu_gain'], u['beta'],
                                       u['kappa'], u['w'], u['theta_star'])
                        ),

                      # 2. Defer-speedup effect ======================================

                      # 2.1.1. Changes in time scaling (kappa) -----------------------
                      # - neutral framing: kappa = 1
                      # - defer framing: kappa > 1 (over-responsive to deferrals)
                      # - speedup framing: kappa < 1 (under-responsive to speedups)

                      defer_speedup_time_scaling =
                        list(lower = c(l['alpha'], l['mu'], l['beta'], l['kappa'],
                                       l['kappa_loss'], l['kappa_gain'], l['w'], l['theta_star']),
                             upper = c(u['alpha'], u['mu'], u['beta'], u['kappa'],
                                       u['kappa_loss'], u['kappa_gain'], u['w'], u['theta_star'])
                        ),

                      # 2.2.1. Changes in value scaling (mu) ---------------------------
                      # - neutral framing:
                      # - defer framing:
                      # - speedup framing:

                      defer_speedup_value_scaling =
                        list(lower = c(l['alpha'], l['mu'], l['mu_loss'], l['beta'],
                                       l['kappa'], l['w'], l['theta_star']),
                             upper = c(u['alpha'], u['mu'], u['mu_loss'], u['beta'],
                                       u['kappa'], u['w'], u['theta_star'])
                        )
         ),
         DFT_CRT = list(one_condition =
                          list(lower = c(l['alpha'], l['mu'], l['beta'], l['kappa'],
                                         l['w'], l['theta_star'], l['t0']),
                               upper = c(u['alpha'], u['mu'], u['beta'], u['kappa'],
                                         u['w'], u['theta_star'], u['t0'])
                          ),

                        # 1. Date-delay effect =========================================

                        # 1.1.1. Changes in time scaling (kappa) -------------------------
                        # - delay framing: kappa = 1
                        # - date framing: kappa = kappa_loss < 1

                        date_delay_time_scaling =
                          list(lower = c(l['alpha'], l['mu'], l['beta'], l['kappa'],
                                         l['kappa_gain'], l['w'], l['theta_star'], l['t0']),
                               upper = c(u['alpha'], u['mu'], u['beta'], u['kappa'],
                                         u['kappa_gain'], u['w'], u['theta_star'], u['t0'])
                          ),

                        # 1.1.2. Changes in time scaling (kappa) & t0 ------------------
                        # - delay framing: kappa = 1, t0 = t01
                        # - date framing: kappa = kappa_loss < 1, t0 = t02

                        date_delay_time_scaling_t0 =
                          list(lower = c(l['alpha'], l['mu'], l['beta'], l['kappa'],
                                         l['kappa_gain'], l['w'], l['theta_star'], l['t0'], l['t0']),
                               upper = c(u['alpha'], u['mu'], u['beta'], u['kappa'],
                                         u['kappa_gain'], u['w'], u['theta_star'], u['t0'], u['t0'])
                          ),

                        # 1.2.1. Changes in value scaling (mu) ---------------------------
                        # - delay framing: mu = 1
                        # - date framing: mu = mu_gain > 1

                        date_delay_value_scaling =
                          list(lower = c(l['alpha'], l['mu'], l['mu_gain'], l['beta'],
                                         l['kappa'], l['w'], l['theta_star'], l['t0']),
                               upper = c(u['alpha'], u['mu'], u['mu_gain'], u['beta'],
                                         u['kappa'], u['w'], u['theta_star'], u['t0'])
                          ),

                        # 1.2.2. Changes in value scaling (mu) ---------------------------
                        # - delay framing: mu = 1, t0 = t01
                        # - date framing: mu = mu_gain > 1, t0 = t02

                        date_delay_value_scaling_t0 =
                          list(lower = c(l['alpha'], l['mu'], l['mu_gain'], l['beta'],
                                         l['kappa'], l['w'], l['theta_star'], l['t0'], l['t0']),
                               upper = c(u['alpha'], u['mu'], u['mu_gain'], u['beta'],
                                         u['kappa'], u['w'], u['theta_star'], u['t0'], u['t0'])
                          ),

                        # 2. Defer-speedup effect ======================================

                        # 2.1.1. Changes in time scaling (kappa) -----------------------
                        # - neutral framing: kappa = 1
                        # - defer framing: kappa > 1 (over-responsive to deferrals)
                        # - speedup framing: kappa < 1 (under-responsive to speedups)

                        defer_speedup_time_scaling =
                          list(lower = c(l['alpha'], l['mu'], l['beta'], l['kappa'],
                                         l['kappa_loss'], l['kappa_gain'], l['w'], l['theta_star'], l['t0']),
                               upper = c(u['alpha'], u['mu'], u['beta'], u['kappa'],
                                         u['kappa_loss'], u['kappa_gain'], u['w'], u['theta_star'], u['t0'])
                          ),

                        # 2.1.2. Changes in time scaling (kappa) -----------------------
                        # - neutral framing: kappa = 1, t0 = t01
                        # - defer framing: kappa > 1 (over-responsive to deferrals), t0 = t02
                        # - speedup framing: kappa < 1 (under-responsive to speedups), t0 = t03

                        defer_speedup_time_scaling_t0 =
                          list(lower = c(l['alpha'], l['mu'], l['beta'], l['kappa'],
                                         l['kappa_loss'], l['kappa_gain'], l['w'],
                                         l['theta_star'], l['t0'], l['t0'], l['t0']),
                               upper = c(u['alpha'], u['mu'], u['beta'], u['kappa'],
                                         u['kappa_loss'], u['kappa_gain'], u['w'],
                                         u['theta_star'], u['t0'], u['t0'], u['t0'])
                          ),

                        # 2.2.1. Changes in value scaling (mu) ---------------------------
                        # - neutral framing:
                        # - defer framing:
                        # - speedup framing:

                        defer_speedup_value_scaling =
                          list(lower = c(l['alpha'], l['mu'], l['mu_loss'], l['beta'],
                                         l['kappa'], l['w'], l['theta_star'], l['t0']),
                               upper = c(u['alpha'], u['mu'], u['mu_loss'], u['beta'],
                                         u['kappa'], u['w'], u['theta_star'], u['t0'])
                          ),

                        # 2.2.2. Changes in value scaling (mu) ---------------------------
                        # - neutral framing:
                        # - defer framing:
                        # - speedup framing:

                        defer_speedup_value_scaling_t0 =
                          list(lower = c(l['alpha'], l['mu'], l['mu_loss'], l['beta'],
                                         l['kappa'], l['w'], l['theta_star'], l['t0'], l['t0'], l['t0']),
                               upper = c(u['alpha'], u['mu'], u['mu_loss'], u['beta'],
                                         u['kappa'], u['w'], u['theta_star'], u['t0'], u['t0'], u['t0'])
                          )


         )
         )[[model]][[parameterization]][[bound]]
    )
}

get_par_pop_stats <- function(model = "DDM", parameterization = "", descstat = "mean") {

  means = list('alpha' = 0.94, # Source: DB_2014 (Table 10, Expt. 3)
                'mu' = 1, # # Guesstimate
                'mu_gain' = 1.2, # Guesstimate
                'mu_loss' = 1.2, # Guesstimate
                'beta' = 0.75, # DB_JEPG_2014
                'kappa' = 1, # Guesstimate
                'kappa_gain' = 1 / 1.2, # Guesstimate
                'kappa_loss' = 1.2, # Guesstimate
                'w' = 0.48, # Source: DB_2014 (Table 10, Expt. 3)
                "a" = 1, # Source: guesstimate based on typical range according to rtdists
                "t0" = 1.23, # # Source: DB_2014 (Table 10, Expt. 3)
                "theta_star" = 1.94 # Source: DB_2014 (Table 10, Expt. 3)
               )

  sds = list('alpha' = 0.64, # Source: DB_2014 (Table 10, Expt. 3)
             'mu' = 1e-6, # # Guesstimate; must be bigger than zero (as diagonals of covariance matrix need to be positive)
             'mu_gain' = 0.1, # Guesstimate
             'mu_loss' = 0.1, # Guesstimate
             'beta' = 0.65, # DB_JEPG_2014
             'kappa' = 1e-6, # Guesstimate; must be bigger than zero (as diagonals of covariance matrix need to be positive)
             'kappa_gain' = 1 / 1.2, # Guesstimate
             'kappa_loss' = 1.2, # Guesstimate
             'w' = 0.18, # Source: DB_2014 (Table 10, Expt. 3)
             "a" = 1, # Guesstimate
             "t0" = 0.14, # # Source: DB_2014 (Table 10, Expt. 3)
             "theta_star" = 0.61, # Source: DB_2014 (Table 10, Expt. 3, a = theta_star / 2)
  )

  M <- unlist(means)
  S <- unlist(sds)

  get_correlation_mat <- function(names) {

  one <- 1
  mkp <- 0.10   # Positive correlation of parameters with mu and kappa
  mkn <- -0.10  # Negtaive correlation of parameters with mu and kappa
  mkw <- 0.85   # Positive correlation within mu-kappa
  alb <- 0.85   # Alpha-beta
  alw <- 0.25   # Alpha-w
  ala <- -0.15  # Alpha-a
  alt <- 0.29   # Alpha-t0
  b_w <- 0.26   # Beta-w
  b_a <- -0.15  # Beta-a
  b_t <- 0.23   # Beta-t0
  w_a <- -0.15  # w-a
  w_t <- 0.04   # w-t0
  a_t <- 0.04   # a-t0


    mat <-
      tibble::frame_matrix(
        ~alpha, ~mu,  ~mu_gain, ~mu_loss, ~beta, ~kappa, ~kappa_gain, ~kappa_loss, ~w,   ~a,    ~t0,
        one,    mkp,  mkp,      mkp,      alb,   mkp,    mkp,         mkp,         alw,  ala,   alt,
        mkp,    one,  mkw,      mkw,      mkp,   mkw,    mkw,         mkw,         mkp,  mkn,   mkp,
        mkp,    mkw,  one,      mkw,      mkp,   mkw,    mkw,         mkw,         mkp,  mkn,   mkp,
        mkp,    mkw,  mkw,      one,      mkp,   mkw,    mkw,         mkw,         mkp,  mkn,   mkp,
        alb,    mkp,  mkp,      mkp,      one,   mkp,    mkp,         mkp,         b_w,  b_a,   b_t,
        mkp,    mkw,  mkw,      mkw,      mkp,   one,    mkw,         mkw,         mkp,  mkn,   mkp,
        mkp,    mkw,  mkw,      mkw,      mkp,   mkw,    one,         mkw,         mkp,  mkn,   mkp,
        mkp,    mkw,  mkw,      mkw,      mkp,   mkw,    mkw,         one,         mkp,  mkn,   mkp,
        alw,    mkp,  mkp,      mkp,      b_w,   mkp,    mkp,         mkp,         one,  w_a,   w_t,
        ala,    mkn,  mkn,      mkn,      b_a,   mkn,    mkn,         mkn,         w_a,  one,   a_t,
        alt,    mkp,  mkp,      mkp,      b_t,   mkp,    mkp,         mkp,         w_t,  a_t,   one
      )

    selection <- colnames(mat) %in% names
    mat[selection, selection]
  }


  unname(
    list(DDM =
           list(one_condition =
                  list(mean = c(M['alpha'], M['mu'], M['beta'], M['kappa'], M['w'], M['a'], M['t0']),
                       sd = c(S['alpha'], S['mu'], S['beta'], S['kappa'], S['w'], S['a'], S['t0']),
                       correlation = get_correlation_mat(names = c('alpha', 'mu', 'beta', 'kappa', 'w', 'a', 't0'))
                  ),

                # 1. Date-delay effect =========================================

                # 1.1.1. Changes in time scaling (kappa) -------------------------
                # - delay framing: kappa = 1
                # - date framing: kappa = kappa_loss < 1

                date_delay_time_scaling =
                  list(mean = c(M['alpha'], M['mu'], M['beta'], M['kappa'], M['kappa_loss'], M['w'], M['a'], M['t0']),
                       sd = c(S['alpha'], S['mu'], S['beta'], S['kappa'], S['kappa_loss'], S['w'], S['a'], S['t0']),
                       correlation = get_correlation_mat(names = c('alpha', 'mu', 'beta', 'kappa', 'kappa_loss', 'w', 'a', 't0'))
                  ),

                # 1.1.2. Changes in time scaling (kappa) & t0 ------------------
                # - delay framing: kappa = 1, t0 = t01
                # - date framing: kappa = kappa_loss < 1, t0 = t01

                date_delay_time_scaling_t0 =
                  list(mean = c(M['alpha'], M['mu'], M['beta'], M['kappa'], M['kappa_loss'], M['w'], M['a'], M['t0'], M['t0']),
                       sd = c(S['alpha'], S['mu'], S['beta'], S['kappa'], S['kappa_loss'], S['w'], S['a'], S['t0'], S['t0']),
                       correlation = get_correlation_mat(names = c('alpha', 'mu', 'beta', 'kappa', 'kappa_loss', 'w', 'a', 't0', 't0'))
                  ),

                # 1.2.1. Changes in value scaling (mu) ---------------------------
                # - delay framing: mu = 1
                # - date framing: mu = mu_gain > 1

                date_delay_value_scaling =
                  list(mean = c(M['alpha'], M['mu'], M['mu_gain'], M['beta'], M['kappa'], M['w'], M['a'], M['t0']),
                       sd = c(S['alpha'], S['mu'], S['mu_gain'], S['beta'], S['kappa'], S['w'], S['a'], S['t0']),
                       correlation = get_correlation_mat(names = c('alpha', 'mu', 'mu_gain', 'beta', 'kappa', 'w', 'a', 't0'))
                  ),

                # 1.2.2. Changes in value scaling (mu) & t0 --------------------
                # - delay framing: mu = 1, t0 = t01
                # - date framing: mu = mu_gain > 1, t0 = t02

                date_delay_value_scaling_t0 =
                  list(mean = c(M['alpha'], M['mu'], M['mu_gain'], M['beta'], M['kappa'], M['w'], M['a'], M['t0'], M['t0']),
                       sd = c(S['alpha'], S['mu'], S['mu_gain'], S['beta'], S['kappa'], S['w'], S['a'], S['t0'], S['t0']),
                       correlation = get_correlation_mat(names = c('alpha', 'mu', 'mu_gain', 'beta', 'kappa', 'w', 'a', 't0', 't0'))
                  ),

                # 2. Defer-speedup effect ======================================

                # 2.1.1. Changes in time scaling (kappa) -----------------------
                # - neutral framing: kappa = 1
                # - defer framing: kappa > 1 (over-responsive to deferrals)
                # - speedup framing: kappa < 1 (under-responsive to speedups)

                defer_speedup_time_scaling =
                  list(mean = c(M['alpha'], M['mu'], M['beta'], M['kappa'], M['kappa_loss'], M['kappa_gain'], M['w'], M['a'], M['t0']),
                       sd = c(S['alpha'], S['mu'], S['beta'], S['kappa'], S['kappa_loss'], S['kappa_gain'], S['w'], S['a'], S['t0']),
                       correlation = get_correlation_mat(names = c('alpha', 'mu', 'beta', 'kappa', 'kappa_loss', 'kappa_gain', 'w', 'a', 't0'))
                  ),

                # 2.1.2. Changes in time scaling (kappa) & t0 ------------------
                # - neutral framing: kappa = 1, t0 = t01
                # - defer framing: kappa > 1 (over-responsive to deferrals), t0 = t02
                # - speedup framing: kappa < 1 (under-responsive to speedups), t0 = t03

                defer_speedup_time_scaling_t0 =
                  list(mean = c(M['alpha'], M['mu'], M['beta'], M['kappa'], M['kappa_loss'], M['kappa_gain'], M['w'], M['a'], M['t0'], M['t0'], M['t0']),
                       sd = c(S['alpha'], S['mu'], S['beta'], S['kappa'], S['kappa_loss'], S['kappa_gain'], S['w'], S['a'], S['t0'], S['t0'], S['t0']),
                       correlation = get_correlation_mat(names = c('alpha', 'mu', 'beta', 'kappa', 'kappa_loss', 'kappa_gain', 'w', 'a', 't0', 't0', 't0'))
                  ),

                # 2.2.1. Changes in value scaling (mu) ---------------------------
                # - neutral framing:
                # - defer framing:
                # - speedup framing:

                defer_speedup_value_scaling =
                  list(mean = c(M['alpha'], M['mu'], M['mu_loss'], M['beta'], M['kappa'], M['w'], M['a'], M['t0']),
                       sd = c(S['alpha'], S['mu'], S['mu_loss'], S['beta'], S['kappa'], S['w'], S['a'], S['t0']),
                       correlation = get_correlation_mat(names = c('alpha', 'mu', 'mu_loss', 'beta', 'kappa', 'w', 'a', 't0'))
                  ),

                # 2.2.2. Changes in value scaling (mu) & t0---------------------
                # - neutral framing:
                # - defer framing:
                # - speedup framing:

                defer_speedup_value_scaling_t0 =
                  list(mean = c(M['alpha'], M['mu'], M['mu_loss'], M['beta'], M['kappa'], M['w'], M['a'], M['t0'], M['t0'], M['t0']),
                       sd = c(S['alpha'], S['mu'], S['mu_loss'], S['beta'], S['kappa'], S['w'], S['a'], S['t0'], S['t0'], S['t0']),
                       correlation = get_correlation_mat(names = c('alpha', 'mu', 'mu_loss', 'beta', 'kappa', 'w', 'a', 't0', 't0', 't0'))
                  )


           )
    )[[model]][[parameterization]][[descstat]]
  )
}




## get_par_values  #############################################################
#' Get parameter values
#'
#' @param x parameters
#' @inheritParams fit_model
#' @export
get_par_values = function(x, model = "DFT_C", parameterization = "") {

  # 1. Define parameter names ==================================================
  parameter_names <-
    get_par_names(model = model,
                  parameterization = "one_condition")
  params = list()
  names(x) = get_par_names(model = model,
                           parameterization = parameterization)

  # 2. Extract parameter values ================================================
  if (model == "DDM") {
    if (parameterization == "one_condition") {
      params = c(x["alpha"], x["mu"], x["beta"], x["kappa"], x["w"], x["a"], x["t0"])
      names(params) = parameter_names

    } else if (parameterization == "date_delay_time_scaling") {

      # 2.1.1. Date/delay effect - changes in time scaling (kappa) ---------------

      # delay frame: kappa = 1
      params[[1]] = c(x["alpha"], x["mu"], x["beta"], x["kappa1"], x["w"], x["a"], x["t0"])

      # date frame: kappa = kappa_loss < 1
      params[[2]] = c(x["alpha"], x["mu"], x["beta"], x["kappa2"], x["w"], x["a"], x["t0"])

      names(params[[1]]) = names(params[[2]]) = parameter_names

    } else if (parameterization == "date_delay_time_scaling_t0") {

      # 2.1.2. Date/delay effect - changes in time scaling (kappa) & t0 ----------

      # delay frame: kappa = 1, t0 = t01
      params[[1]] = c(x["alpha"], x["mu"], x["beta"], x["kappa1"], x["w"], x["a"], x["t01"])

      # date frame: kappa = kappa_loss < 1, t0 = t02
      params[[2]] = c(x["alpha"], x["mu"], x["beta"], x["kappa2"], x["w"], x["a"], x["t02"])

      names(params[[1]]) = names(params[[2]]) = parameter_names

    } else if (parameterization == "date_delay_value_scaling") {

      # 2.2.1. Date/delay effect - changes in value scaling (mu) -----------------

      # delay frame: mu = 1
      params[[1]] = c(x["alpha"], x["mu1"], x["beta"], x["kappa"], x["w"], x["a"], x["t0"])

      # date frame: mu = mu_gain > 1
      params[[2]] = c(x["alpha"], x["mu2"], x["beta"], x["kappa"], x["w"], x["a"], x["t0"])
      names(params[[1]]) = names(params[[2]]) = parameter_names

    } else if (parameterization == "date_delay_value_scaling_t0") {

      # 2.2.2. Date/delay effect - changes in value scaling (mu) & t0 ------------

      # delay frame: mu = 1, t0 = t01
      params[[1]] = c(x["alpha"], x["mu1"], x["beta"], x["kappa"], x["w"], x["a"], x["t01"])

      # date frame: mu = mu_gain > 1, t0 = t02
      params[[2]] = c(x["alpha"], x["mu2"], x["beta"], x["kappa"], x["w"], x["a"], x["t02"])
      names(params[[1]]) = names(params[[2]]) = parameter_names

    } else if (parameterization == "defer_speedup_time_scaling") {

      # 2.3.1. Defer/speedup effect - changes in time scaling (kappa) --------------

      # neutral frame: kappa = 1
      params[[1]] = c(x["alpha"], x["mu"], x["beta"], x["kappa1"], x["w"], x["a"], x["t0"])

      # defer frame: kappa = 1
      params[[2]] = c(x["alpha"], x["mu"], x["beta"], x["kappa2"], x["w"], x["a"], x["t0"])

      # speedup frame: kappa = 1
      params[[3]] = c(x["alpha"], x["mu"], x["beta"], x["kappa3"], x["w"], x["a"], x["t0"])

      names(params[[1]]) = names(params[[2]]) = names(params[[3]]) = parameter_names

    } else if (parameterization == "defer_speedup_time_scaling_t0") {

      # 2.3.2. Defer/speedup effect - changes in time scaling (kappa) & t0 -------

      # neutral frame: kappa = 1
      params[[1]] = c(x["alpha"], x["mu"], x["beta"], x["kappa1"], x["w"], x["a"], x["t01"])

      # defer frame: kappa = 1
      params[[2]] = c(x["alpha"], x["mu"], x["beta"], x["kappa2"], x["w"], x["a"], x["t02"])

      # speedup frame: kappa = 1
      params[[3]] = c(x["alpha"], x["mu"], x["beta"], x["kappa3"], x["w"], x["a"], x["t03"])

      names(params[[1]]) = names(params[[2]]) = names(params[[3]]) = parameter_names

    } else if (parameterization == "defer_speedup_value_scaling") {

      # 2.4.1. Defer/speedup effect - changes in value scaling (mu) --------------

      # neutral frame: mu = 1
      params[[1]] = c(x["alpha"], x["mu1"], x["beta"], x["kappa"], x["w"], x["a"], x["t0"])

      # defer frame: mu > 1 for
      params[[2]] = c(x["alpha"], x["mu2"], x["beta"], x["kappa"], x["w"], x["a"], x["t0"])

      # speedup frame: mu > 1 for
      params[[3]] = c(x["alpha"], x["mu2"], x["beta"], x["kappa"], x["w"], x["a"], x["t0"])

      names(params[[1]]) = names(params[[2]]) = names(params[[3]]) = parameter_names
    } else if (parameterization == "defer_speedup_value_scaling_t0") {

      # 2.4.2. Defer/speedup effect - changes in value scaling (mu) & t0 ---------

      # neutral frame: mu = 1, t0 = t01
      params[[1]] = c(x["alpha"], x["mu1"], x["beta"], x["kappa"], x["w"], x["a"], x["t01"])

      # defer frame: mu > 1 for, t0 = t02
      params[[2]] = c(x["alpha"], x["mu2"], x["beta"], x["kappa"], x["w"], x["a"], x["t02"])

      # speedup frame: mu > 1 for, t0 = t03
      params[[3]] = c(x["alpha"], x["mu2"], x["beta"], x["kappa"], x["w"], x["a"], x["t03"])

      names(params[[1]]) = names(params[[2]]) = names(params[[3]]) = parameter_names
    }
  } else if (model == "DFT_C") {
    if (parameterization == "one_condition") {
      params = c(x["alpha"], x["mu"], x["beta"], x["kappa"], x["w"], x["theta_star"])
      names(params) = parameter_names

    } else if (parameterization == "date_delay_time_scaling") {

      # 2.1.1. Date/delay effect - changes in time scaling (kappa) ---------------

      # delay frame: kappa = 1
      params[[1]] = c(x["alpha"], x["mu"], x["beta"], x["kappa1"], x["w"], x["theta_star"])

      # date frame: kappa = kappa_loss < 1
      params[[2]] = c(x["alpha"], x["mu"], x["beta"], x["kappa2"], x["w"], x["theta_star"])

      names(params[[1]]) = names(params[[2]]) = parameter_names

    } else if (parameterization == "date_delay_value_scaling") {

      # 2.2.1. Date/delay effect - changes in value scaling (mu) -----------------

      # delay frame: mu = 1
      params[[1]] = c(x["alpha"], x["mu1"], x["beta"], x["kappa"], x["w"], x["theta_star"])

      # date frame: mu = mu_gain > 1
      params[[2]] = c(x["alpha"], x["mu2"], x["beta"], x["kappa"], x["w"], x["theta_star"])
      names(params[[1]]) = names(params[[2]]) = parameter_names

    } else if (parameterization == "defer_speedup_time_scaling") {

      # 2.3.1. Defer/speedup effect - changes in time scaling (kappa) --------------

      # neutral frame: kappa = 1
      params[[1]] = c(x["alpha"], x["mu"], x["beta"], x["kappa1"], x["w"], x["theta_star"])

      # defer frame: kappa = 1
      params[[2]] = c(x["alpha"], x["mu"], x["beta"], x["kappa2"], x["w"], x["theta_star"])

      # speedup frame: kappa = 1
      params[[3]] = c(x["alpha"], x["mu"], x["beta"], x["kappa3"], x["w"], x["theta_star"])

      names(params[[1]]) = names(params[[2]]) = names(params[[3]]) = parameter_names

    } else if (parameterization == "defer_speedup_value_scaling") {

      # 2.4.1. Defer/speedup effect - changes in value scaling (mu) --------------

      # neutral frame: mu = 1
      params[[1]] = c(x["alpha"], x["mu1"], x["beta"], x["kappa"], x["w"], x["theta_star"])

      # defer frame: mu > 1 for
      params[[2]] = c(x["alpha"], x["mu2"], x["beta"], x["kappa"], x["w"], x["theta_star"])

      # speedup frame: mu > 1 for
      params[[3]] = c(x["alpha"], x["mu2"], x["beta"], x["kappa"], x["w"], x["theta_star"])

      names(params[[1]]) = names(params[[2]]) = names(params[[3]]) = parameter_names
    }
  } else if (model == "DFT_CRT") {
    if (parameterization == "one_condition") {
      params = c(x["alpha"], x["mu"], x["beta"], x["kappa"], x["w"], x["theta_star"], x["t0"])
      names(params) = parameter_names

    } else if (parameterization == "date_delay_time_scaling") {

      # 2.1.1. Date/delay effect - changes in time scaling (kappa) ---------------

      # delay frame: kappa = 1
      params[[1]] = c(x["alpha"], x["mu"], x["beta"], x["kappa1"], x["w"], x["theta_star"], x["t0"])

      # date frame: kappa = kappa_loss < 1
      params[[2]] = c(x["alpha"], x["mu"], x["beta"], x["kappa2"], x["w"], x["theta_star"], x["t0"])

      names(params[[1]]) = names(params[[2]]) = parameter_names

    } else if (parameterization == "date_delay_time_scaling_t0") {

      # 2.1.2. Date/delay effect - changes in time scaling (kappa) & t0 ----------

      # delay frame: kappa = 1, t0 = t01
      params[[1]] = c(x["alpha"], x["mu"], x["beta"], x["kappa1"], x["w"], x["theta_star"], x["t01"])

      # date frame: kappa = kappa_loss < 1, t0 = t02
      params[[2]] = c(x["alpha"], x["mu"], x["beta"], x["kappa2"], x["w"], x["theta_star"], x["t02"])

      names(params[[1]]) = names(params[[2]]) = parameter_names

    } else if (parameterization == "date_delay_value_scaling") {

      # 2.2.1. Date/delay effect - changes in value scaling (mu) -----------------

      # delay frame: mu = 1
      params[[1]] = c(x["alpha"], x["mu1"], x["beta"], x["kappa"], x["w"], x["theta_star"], x["t0"])

      # date frame: mu = mu_gain > 1
      params[[2]] = c(x["alpha"], x["mu2"], x["beta"], x["kappa"], x["w"], x["theta_star"], x["t0"])
      names(params[[1]]) = names(params[[2]]) = parameter_names

    } else if (parameterization == "date_delay_value_scaling_t0") {

      # 2.2.2. Date/delay effect - changes in value scaling (mu) & t0 ------------

      # delay frame: mu = 1, t0 = t01
      params[[1]] = c(x["alpha"], x["mu1"], x["beta"], x["kappa"], x["w"], x["theta_star"], x["t01"])

      # date frame: mu = mu_gain > 1, t0 = t02
      params[[2]] = c(x["alpha"], x["mu2"], x["beta"], x["kappa"], x["w"], x["theta_star"], x["t02"])
      names(params[[1]]) = names(params[[2]]) = parameter_names

    } else if (parameterization == "defer_speedup_time_scaling") {

      # 2.3.1. Defer/speedup effect - changes in time scaling (kappa) --------------

      # neutral frame: kappa = 1
      params[[1]] = c(x["alpha"], x["mu"], x["beta"], x["kappa1"], x["w"], x["theta_star"], x["t0"])

      # defer frame: kappa = 1
      params[[2]] = c(x["alpha"], x["mu"], x["beta"], x["kappa2"], x["w"], x["theta_star"], x["t0"])

      # speedup frame: kappa = 1
      params[[3]] = c(x["alpha"], x["mu"], x["beta"], x["kappa3"], x["w"], x["theta_star"], x["t0"])

      names(params[[1]]) = names(params[[2]]) = names(params[[3]]) = parameter_names

    } else if (parameterization == "defer_speedup_time_scaling_t0") {

      # 2.3.2. Defer/speedup effect - changes in time scaling (kappa) & t0 -------

      # neutral frame: kappa = 1
      params[[1]] = c(x["alpha"], x["mu"], x["beta"], x["kappa1"], x["w"], x["theta_star"], x["t01"])

      # defer frame: kappa = 1
      params[[2]] = c(x["alpha"], x["mu"], x["beta"], x["kappa2"], x["w"], x["theta_star"], x["t02"])

      # speedup frame: kappa = 1
      params[[3]] = c(x["alpha"], x["mu"], x["beta"], x["kappa3"], x["w"], x["theta_star"], x["t03"])

      names(params[[1]]) = names(params[[2]]) = names(params[[3]]) = parameter_names

    } else if (parameterization == "defer_speedup_value_scaling") {

      # 2.4.1. Defer/speedup effect - changes in value scaling (mu) --------------

      # neutral frame: mu = 1
      params[[1]] = c(x["alpha"], x["mu1"], x["beta"], x["kappa"], x["w"], x["theta_star"], x["t0"])

      # defer frame: mu > 1 for
      params[[2]] = c(x["alpha"], x["mu2"], x["beta"], x["kappa"], x["w"], x["theta_star"], x["t0"])

      # speedup frame: mu > 1 for
      params[[3]] = c(x["alpha"], x["mu2"], x["beta"], x["kappa"], x["w"], x["theta_star"], x["t0"])

      names(params[[1]]) = names(params[[2]]) = names(params[[3]]) = parameter_names
    } else if (parameterization == "defer_speedup_value_scaling_t0") {

      # 2.4.2. Defer/speedup effect - changes in value scaling (mu) & t0 ---------

      # neutral frame: mu = 1, t0 = t01
      params[[1]] = c(x["alpha"], x["mu1"], x["beta"], x["kappa"], x["w"], x["theta_star"], x["t01"])

      # defer frame: mu > 1 for, t0 = t02
      params[[2]] = c(x["alpha"], x["mu2"], x["beta"], x["kappa"], x["w"], x["theta_star"], x["t02"])

      # speedup frame: mu > 1 for, t0 = t03
      params[[3]] = c(x["alpha"], x["mu2"], x["beta"], x["kappa"], x["w"], x["theta_star"], x["t03"])

      names(params[[1]]) = names(params[[2]]) = names(params[[3]]) = parameter_names
    }
  }

  # 3. Return parameter values =================================================
  params
}



## itch_ddm ###################################################################
#' Intertemporal choice drift diffusion model
#'
#' @param stimuli bla # TODO: use @inher@inheritParams
#' @param parameters ble # TODO: use @inher@inheritParams
#' @export
#' This model is inspired by Dai & Busemeyer, J Exp Psychol Gen, 2014 and Scholten & Read, J Exp Psychol Learn Mem Cogn, 2013
itch_ddm <- function(stimuli, parameters, parameterization = "", frame = "", n = 1, sim_fun = 'rtdists_package') {

  # 1. Unlist parameters =======================================================
  x = unlist(parameters)

  # 2. Compute drift rate ======================================================
  v <- compute_drift_rate(parameters = parameters,
                          stimuli = stimuli,
                          parameterization = parameterization,
                          frame = frame)

  # 3. Compute and return predicted responses ==================================
  # Either using Dai & Busemeyer's method (Eq. 9 in their paper) or using the rtdists packages

  if (sim_fun == 'dai_busemeyer') {
    tibble::tibble(p_ll = itchmodel::db_bin_choice_prob(d = v,
                                                        s = 0.1,
                                                        a = unname(x['a']),
                                                        z = 0))
  } else if (sim_fun == 'rtdists_package') {
    purrr::map_df(.x = v,
                  .f = rtdists::rdiffusion,
                  n = n,
                  s = 0.1,
                  a = x['a'],
                  t0 = x['t0']
    ) %>%
      dplyr::mutate(v = v)
  }
}



## ll_dft ################################################################
#' Log-likelihood for decision field theory
#'
#' This is based on Junyi Dai's intertemporal choice code
#'
#' @export
ll_dft <- function(x, stimuli, frame = "", observations, rt = TRUE) {

  # 1. Unlist parameters =======================================================
  x <- unlist(x)

  # 2. Compute drift rate ======================================================
  du <- compute_transformation_diffs(parameters = parameters,
                                     stimuli = stimuli,
                                     parameterization = parameterization,
                                     frame = frame,
                                     variable = 'du')

  dp <- compute_transformation_diffs(parameters = parameters,
                                     stimuli = stimuli,
                                     parameterization = parameterization,
                                     frame = frame,
                                     variable = 'dp')

  d <- unname(x["w"] * du - (1 - x["w"]) * dp)

  # Probability of sampling from or attending to a specific attribute at any
  # time is assumed to equal the corresponding attention weight. See Eq. 15 in
  # Dai & Busemeyer
  s = unname(sqrt(x["w"] * du^2 + (1 - x["w"]) * dp^2 - d^2))

  # 3. Compute densities =======================================================
  if (rt) {
    densities <-
      tryCatch(dft_dpd(d = d,
                       s = s,
                       theta = unname(x["theta_star"] * s),
                       z = x["z"],
                       response = observations$response,
                       rtd = observations$rt - x["t0"]),
               error = function(e) 0)
  } else {
    densities <-
      tryCatch(dft_cp(d = d,
                      s = s,
                      theta = unname(x["theta_star"] * s),
                      z = x["z"],
                      response = observations$response),
               error = function(e) 0)

  }

  if (any(densities == 0)) return(1e6)
  return(-sum(log(densities)))

}


## ll_diffusion ################################################################
#' Log-likelihood for drift diffusion model
#'
#' This is based on example in rtdists::ddiffusion package
#'
#' @param pars Vector of parameters
#' @inheritParams rtdists::ddiffusion
#' @export
ll_diffusion <- function(x, stimuli, frame = "", observations) {

  # 1. Unlist parameters =======================================================
  x <- unlist(x)

  # 2. Compute drift rate ======================================================
  v <- compute_drift_rate(x, stimuli,
                          frame = frame)

  # 3. Compute densities =======================================================
  # Compute densities

  densities <-
    tryCatch(rtdists::ddiffusion(rt = observations$rt,
                                 response = observations$response,
                                 v = v,
                                 s = 0.1,
                                 a = x['a'],
                                 t0 = x['t0']),
             error = function(e) 0)

  if (any(densities == 0)) return(1e6)
  return(-sum(log(densities)))
}

## logistic ####################################################################
#' Logistic function
#'
#' @param x data
#' @export
logistic <- function(x) {
  1 / (1 + exp(-x))
}

## make_stimulus_df ############################################################
#' Make stimulus list (i.e. trials)
#'
#' @param frames character vector of frame names, defaults to c('delay', 'date')
#' @param m_l double vector of monetary amounts of larger option (in Euros), defaults to c(20)
#' @param pct_m_l double vector of monetary amount of smaller option, experessed as percentage of larger option, defaults to c(0.2, 0.5, 0.7, 0.8, 0.88, 0.93, 0.97, 0.99)
#' @param t_s double vector of delay of reward delivery for sooner option (in days), defaults to c(0)
#' @param interval double vector of intervals between sooner and later option (in days), defaults to c(0)
#' @param n_reps number of repetitions of each combination of parameters
#' @return The output is a tibble with columns
make_stimulus_df <- function(frames = "neutral",
                             m_l = c(20),
                             pct_m_l = c(0.2, 0.5, 0.7, 0.8, 0.88, 0.93, 0.97, 0.99),
                             t_s = c(0),
                             interval = c(1,2,3,5,7,10,14),
                             n_reps = 1) {

  if (all(c('delay', 'date') %in% frames)) {
    factor_levels <- c('delay','date')
  } else if (all(c('neutral', 'defer', 'speedup') %in% frames)) {
    factor_levels <- c('neutral', 'defer', 'speedup')
  } else { # e.g. for indifference point procedure
    factor_levels <- 'neutral'
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

## nest_join_stimuli_parameters ################################################
#' Nest and join stimulu and parameters
#'
#' nest_join_stimuli_parameters
#'
nest_join_stimuli_parameters <- function(stimuli = make_stimulus_df(),
                                         parameters, parameterization = "") {

  # Assertions -----------------------------------------------------------------
  assertthat::assert_that(parameterization %in% c("one_condition",
                                                  "date_delay_value_scaling",
                                                  "date_delay_value_scaling_t0",
                                                  "defer_speedup_value_scaling",
                                                  "defer_speedup_value_scaling_t0",
                                                  "date_delay_time_scaling",
                                                  "date_delay_time_scaling_t0",
                                                  "defer_speedup_time_scaling",
                                                  "defer_speedup_time_scaling_t0"
  ),
  msg = "Specify parameterization as \n\"one-condition\" (i.e., estimate parameters from a single experimental condition), \n\"date_delay_value_scaling\" (i.e., estimate free value function scaling parameter across two experimental conditions), \n\"date_delay_value_scaling_t0\" (i.e., estimate free value function scaling parameter and t0 across two experimental conditions), \n\"date_delay_time_scaling\" (i.e., estimate free time function sensitivity parameter across two experimental conditions), \n\"date_delay_time_scaling_t0\" (i.e., estimate free time function sensitivity parameter and t0 across two experimental conditions), \n\"defer_speedup_value_scaling\" (i.e., estimate free value function scaling parameter across two experimental conditions), \n\"defer_speedup_value_scaling_t0\" (i.e., estimate free value function scaling parameter and t0 across two experimental conditions), \n\"defer_speedup_time_scaling\" (i.e., estimate free time function scaling parameter  across two experimental conditions), or \n\"defer_speedup_time_scaling_t0\" (i.e., estimate free time function scaling parameter and t0 across two experimental conditions)."
  )

  # Repeat parameters, if stationary across conditions -------------------------
  if (parameterization %in% c("one-condition",
                              "date_delay_value_sensitivity",
                              "date_delay_value_sensitivity_t0",
                              "defer_speedup_value_scaling",
                              "defer_speedup_value_scaling_t0",
                              "date_delay_time_scaling",
                              "date_delay_time_scaling_t0",
                              "defer_speedup_time_scaling",
                              "defer_speedup_time_scaling_t0")
  ) {
    # parameters <- eq_list_elements(parameters,
    #                                n_cond = length(levels(stimuli$frame)))
  }

  # Nested stimuli -------------------------------------------------------------
  nested_stimuli <-
    stimuli %>%
    dplyr::group_by(frame) %>%
    tidyr::nest(.key = 'stimuli')

  # Nested parameters ----------------------------------------------------------
  nested_parameters <-
    parameters %>%
    get_par_values(model = "DDM", parameterization = parameterization) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(frame = factor(levels(stimuli$frame),
                                 levels = levels(stimuli$frame))) %>%
    dplyr::select(frame, dplyr::everything()) %>%
    dplyr::group_by(frame) %>%
    tidyr::nest(.key = 'parameters')

  # This is the old code:
  # nested_parameters <-
  #   parameters %>%
  #   tibble::as.tibble() %>%
  #   dplyr::mutate(frame = factor(levels(stimuli$frame),
  #                                levels = levels(stimuli$frame))) %>%
  #   dplyr::select(frame, dplyr::everything()) %>%
  #   dplyr::group_by(frame) %>%
  #   tidyr::nest(.key = 'parameters')


  # Join nested stimuli and parameters -----------------------------------------
  nested_stimuli %>%
    dplyr::left_join(nested_parameters, by = "frame")

}


## params_to_tibble ############################################################
#' Convert vector of parameters to a tibble with named columns
#'
#' @param x vector or parameters
#' @param model model name
#' @param parameterization model parameterization
params_to_tibble <- function(x, model = "", parameterization = "") {
  par_names <-
    itchmodel::get_par_names(model = model,
                             parameterization = parameterization)

  params <-
    matrix(x,
           ncol = length(par_names)
    )
  colnames(params) <-
    par_names

  params %>%
    tibble::as.tibble()
}

## sample_params_from_pop ######################################################
#' Sample model parameters from population
#'
#' @export
sample_params_from_pop <- function(n, model = "DDM", parameterization = "", stimuli) {

  # Get parameter population means, standard deviations, and correlations ------
  M <- get_par_pop_stats(model = model,
                         parameterization = parameterization,
                         descstat = "mean")
  S <- get_par_pop_stats(model = model,
                         parameterization = parameterization,
                         descstat = "sd")
  correlation_mat <- get_par_pop_stats(model = model,
                                       parameterization = parameterization,
                                       descstat = "correlation")


  # Parameter covariance matrix ------------------------------------------------
  stdev_ij <- S %*% t(S)
  cov_mat <- correlation_mat * stdev_ij

  # Sample parameters from truncated multivariate normal -----------------------
  # constrained by lower and upper bounds + linear inequalities

  iter <- 1
  params <- double()

  par_names <-
    itchmodel::get_par_names(model = model,
                             parameterization = parameterization)

  print(sprintf("Parameterization: %s", parameterization))

  while (iter <= 50) {

    # Sample parameters with bound constraints
    params <-
      rbind(params,
            tmvtnorm::rtmvnorm2(n = n,
                                mean = M,
                                sigma = cov_mat,
                                lower = get_par_bounds(model = model,
                                                       parameterization = parameterization,
                                                       bound = "lower"),
                                upper = get_par_bounds(model = model,
                                                       parameterization = parameterization,
                                                       bound = "upper") + 1e-6,
                                )
            )

    # Check linear inequalities
    params_list <-
      purrr::array_branch(array = params,
                          margin = 1)

    nested_stimuli <-
      stimuli %>%
      dplyr::group_by(frame) %>%
      tidyr::nest() %>%
      dplyr::rename(stimuli = data)

    lineqs <-
      purrr::pmap_dbl(.l = list(x = params_list),
                      .f = get_nonlinear_constraints,
                      data = nested_stimuli,
                      model = model,
                      parameterization = parameterization
      )

    # Only retain parameter samples that meet linear inequality constraints
    params <- params[lineqs <= 0,]


    # Add column names
    params <- matrix(params, ncol = length(par_names))
    colnames(params) <- par_names

    if (nrow(params) >= n) {

      params <- params[1:n,]
      print(sprintf('Iteration %d: Sampled %d out of %d valid parameter sets', iter, nrow(params), n))
      break

    } else {
      print(sprintf('Iteration %d: Sampled %d out of %d valid parameter sets', iter, nrow(params), n))
      iter <- iter + 1

    }

  }

  # Add column names
  params <- matrix(params, ncol = length(par_names))
  colnames(params) <- par_names

  assertthat::assert_that(nrow(params) >= n,
                          msg = "Algorithm failed to sampled enough valid parameter sets. Consider adjusting settings.")

  # Add index & parameterization column and return
  params <-
    params %>%
    tibble::as.tibble() %>%
    dplyr::mutate(ix = 1:n())

  params %>%
    dplyr::group_by(ix) %>%
    tidyr::nest()

}

## sim_data ####################################################################
#' Simulate intertemporal choice task performance
#'
#' sim_data
#'
#' @param stimuli tibble with trial stimuli
#' @param parameters list with model parameters
#' @param parameterization char vectore specifying model parameterization, defaults to "date_delay_time_scaling"
sim_data <- function(stimuli = make_stimulus_df(),
                     parameters, parameterization = "date_delay_time_scaling", n = 1, sim_fun = 'rtdists_package') {

  # Simulate observations
  nest_join_stimuli_parameters(stimuli = stimuli,
                               parameters = parameters,
                               parameterization = parameterization) %>%
    dplyr::mutate(observations = purrr::pmap(.l = list(stimuli = stimuli,
                                                       parameters = parameters,
                                                       parameterization = parameterization,
                                                       frame = as.character(frame)),
                                             .f = itch_ddm,
                                             n = n,
                                             sim_fun = sim_fun),
                  model = parameterization
    )

}

# sim_n_datasets(stimuli = make_stimulus_df(),
#                parameters, parameterization = "date_delay_time_scaling", n_dataset = 1, n_trial_per_dataset = 1, sim_fun = 'rtdists_package')) {
#
#
#
#
#                  sim_data <- function(stimuli = make_stimulus_df(),
#                                       parameters,
#                                       parameterization = parameterization,
#                                       n = 1,
#                                       sim_fun = sim_fun)
#
#
#
#
#
#                }

