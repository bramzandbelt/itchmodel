# Source some code from John Kruschke for Bayesian logistic regression
source('/Users/bramzandbelt/surfdrive/projects/BEWITCHING/itchmodel/R/Kruschke_code/Jags-Ydich-XmetMulti-Mlogistic.R')
source('/Users/bramzandbelt/surfdrive/projects/BEWITCHING/itchmodel/R/Kruschke_code/DBDA2E-utilities.R')

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

#' #'
#' #'
#' #'
#' compute_drift_rate_transformation_diffs <- function(parameters, stimuli, parameterization = "", frame = "") {
#'
#'   x <- unlist(parameters)
#'
#'   tibble::tibble(variable = character(),
#'                    value = double(),
#'                    weighted = logical()) %>%
#'     tibble::add_row(variable = 'du',
#'                     value = compute_transformation_diffs(parameters = parameters,
#'                                                          stimuli = stimuli,
#'                                                          parameterization = parameterization,
#'                                                          frame = frame,
#'                                                          variable = 'du'),
#'                     weighted = FALSE) %>%
#'     tibble::add_row(variable = 'dp',
#'                     value = compute_transformation_diffs(parameters = parameters,
#'                                                          stimuli = stimuli,
#'                                                          parameterization = parameterization,
#'                                                          frame = frame,
#'                                                          variable = 'dp'),
#'                     weighted = FALSE) %>%
#'     tibble::add_row(variable = 'du',
#'                     value = unname(x['w']) *
#'                       compute_transformation_diffs(parameters = parameters,
#'                                                    stimuli = stimuli,
#'                                                    parameterization = parameterization,
#'                                                    frame = frame,
#'                                                    variable = 'du'),
#'                     weighted = TRUE) %>%
#'     tibble::add_row(variable = 'dp',
#'                     value = (1 - unname(x['w'])) *
#'                       compute_transformation_diffs(parameters = parameters,
#'                                                    stimuli = stimuli,
#'                                                    parameterization = parameterization,
#'                                                    frame = frame,
#'                                                    variable = 'dp'),
#'                     weighted = TRUE) %>%
#'     dplyr::mutate(variable = factor(variable, levels = c('du', 'dp')),
#'                   weighted = factor(weighted, levels = c('FALSE', 'TRUE'))
#'                   )
#'
#'
#' }


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

      NA

    }

  } else if (variable == 'u_ll') {

    # 2.3. Utility of large-but-later option (neutral, defer, speedup frames) --

    if (frame %in% c('neutral','defer')) {

      unname(convert(q, type = 'power', param = c(x['alpha'], 1)))

    } else if (frame == 'speedup') {

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
#' @export
db_bin_choice_prob <- function(d, s, a, z) {

  theta <- 2 * a

  (1 - exp(-2 * d * (theta + z) / s^2)) /
    (1 - exp(-4 * d * theta / s^2))
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
#' @param model Name of the model. Currently, only DDM is supported.
#' @param parameterization Name of the parameterization. Currently, there are five possibilities: (1) date_delay_time_scaling; (2) date_delay_value_scaling; (3) defer_speedup_value_scaling; (4) defer_speedup_time_scaling, and (5) one_condition.
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

  assertthat::assert_that(model %in% c('DDM'),
                          msg = "model is ill-specified. Currently, only the following model is supported: \n'DDM'")

  assertthat::assert_that(parameterization %in% c('one_condition',
                                                  'date_delay_time_scaling',
                                                  'date_delay_value_scaling',
                                                  'defer_speedup_time_scaling',
                                                  'defer_speedup_value_scaling'),
                          msg = "parameterization is ill-specified. It should be any of the following: \n'date_delay_time_scaling', \n'date_delay_value_scaling', \n'defer_speedup_time_scaling', \n'defer_speedup_value_scaling', or \n'one_condition', .")

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
#' DEoptim minimizes negative log likelihood. get_fit_stats computes log likelihood (LL), Akaike Information Criterion (AIC), and Bayesian Information Criterion (BIC) based on that.
#'
#' @export
get_fit_stats <- function(optim_output, model = "", parameterization = "", n_data_points) {

  # Number of free parameters
  n_free_param <- get_n_free_param(model = model,
                                   parameterization = parameterization)

  # Log-likelihood
  LL <- -optim_output$optim$bestval

  # Return fit statistics
  tibble::tibble(model = model,
                 parameterization = parameterization,
                 n_iter = optim_output$optim$iter,
                 n_feval = optim_output$optim$nfeval,
                 bestval = optim_output$optim$bestval,
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
                                                  "date_delay_value_sensitivity",
                                                  "defer_speedup_value_scaling",
                                                  "date_delay_time_scaling",
                                                  "defer_speedup_time_scaling"
  ),
  msg = "Specify parameterization as \n\"one-condition\" (i.e., estimate parameters from a single experimental condition), \n\"date_delay_value_sensitivity\" (i.e., estimate free value function sensitivity parameter across two experimental conditions), \n\"date_delay_time_scaling\" (i.e., estimate free time function sensitivity parameter across two experimental conditions), \n\"defer_speedup_value_scaling\" (i.e., estimate free value function scaling parameter across two experimental conditions), or \n\"defer_speedup_time_scaling\" (i.e., estimate free time function scaling parameter  across two experimental conditions)."
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

## get_log_likelihood  #########################################################
#' Get the log-likelihood of the parameters, given the data
#'
#' This function is a wrapper around ll_diffusion.
#'
#' @param x parameters
#' @inheritParams fit_model
#' @export
get_log_likelihood = function(x, data, model = "DDM", parameterization = "") {

  # 1. Get parameter values ==================================================
  params <- get_par_values(x, model = model, parameterization = parameterization)

  # 2. Return negative log-likelihood for function minimization ================
  ll <- 0

  for (i_cond in 1:length(params)) {
    ll <-
      ll +
      ll_diffusion(x = params[[i_cond]],
                   stimuli = data$stimuli[[i_cond]],
                   frame = data$frame[[i_cond]],
                   observations = data$observations[[i_cond]])
  }

  ll

  # if (parameterization == "one_condition") {
  #   ll_diffusion(x = params,
  #                stimuli = data$stimuli[[1]],
  #                frame = data$frame[[1]],
  #                observations = data$observations[[1]])
  # } else if (parameterization %in% c("date_delay_time_scaling", "date_delay_value_scaling")) {
  #   ll_diffusion(x = params[[1]],
  #                stimuli = data$stimuli[[1]],
  #                frame = data$frame[[1]],
  #                observations = data$observations[[1]]) +
  #     ll_diffusion(x = params[[2]],
  #                  stimuli = data$stimuli[[2]],
  #                  frame = data$frame[[2]],
  #                  observations = data$observations[[2]])
  # } else if (parameterization %in% c("defer_speedup_time_scaling", "defer_speedup_value_scaling")) {
  #   ll_diffusion(x = params[[1]],
  #                stimuli = data$stimuli[[1]],
  #                frame = data$frame[[1]],
  #                observations = data$observations[[1]]) +
  #     ll_diffusion(x = params[[2]],
  #                  stimuli = data$stimuli[[2]],
  #                  frame = data$frame[[2]],
  #                  observations = data$observations[[2]]) +
  #     ll_diffusion(x = params[[3]],
  #                  stimuli = data$stimuli[[3]],
  #                  frame = data$frame[[3]],
  #                  observations = data$observations[[3]])
  #
  # } else
  #   NA

}



## get_par_names ###############################################################
#' Get parameter names, given a model and parameterization
#'
#' @inheritParams fit_model
#' @export
get_par_names = function(model = "DDM", parameterization = "") {

  # 1. Get model parameter names ===============================================
  list(DDM =
         list(
           # 1.1. One condition ------------------------------------------------
           one_condition = c("alpha", "mu", "beta", "kappa", "w", "a", "t0"),

           # 1.2. Date/delay effect - changes in time scaling (kappa) ----------
           # - delay framing: kappa = 1
           # - date framing: kappa = kappa_contraction < 1

           date_delay_time_scaling = c("alpha", "mu", "beta", "kappa1", "kappa2", "w", "a", "t0"),

           # 1.3. Date/delay effect - changes in value scaling (mu) ------------
           # - delay framing: mu = 1
           # - date framing: mu = mu_boost > 1

           date_delay_value_scaling = c("alpha", "mu1", "mu2", "beta", "kappa", "w", "a", "t0"),

           # 1.4. Defer/speedup effect - changes in time scaling (kappa) -------
           # - neutral framing: kappa = 1
           # - defer framing: kappa > 1 (over-responsive to deferrals)
           # - speedup framing: kappa < 1 (under-responsive to speedups)

           defer_speedup_time_scaling = c("alpha", "mu", "beta", "kappa1", "kappa2", "kappa3", "w", "a", "t0"),

           # 1.5. Defer/speedup effect - changes in value scaling (mu) ---------
           # - neutral framing:
           # - defer framing:
           # - speedup framing:

           defer_speedup_value_scaling = c("alpha", "mu1", "mu2", "beta", "kappa", "w", "a", "t0")
         )
  )[[model]][[parameterization]]

}

## get_n_free_param ##############################################################
#' Get number of free parameters, given parameterization
#'
#' @inheritParams get_par_bounds
#' @export
get_n_free_param = function(model = "DDM", parameterization = "") {

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

## get_par_names ###############################################################
#' Get parameter names, given a model and parameterization
#'
#' @inheritParams fit_model
#' @export
get_par_names = function(model = "DDM", parameterization = "") {

  # 1. Get model parameter names ===============================================
  list(DDM =
         list(
           # 1.1. One condition ------------------------------------------------
           one_condition = c("alpha", "mu", "beta", "kappa", "w", "a", "t0"),

           # 1.2. Date/delay effect - changes in time scaling (kappa) ----------
           # - delay framing: kappa = 1
           # - date framing: kappa = kappa_contraction < 1

           date_delay_time_scaling = c("alpha", "mu", "beta", "kappa1", "kappa2", "w", "a", "t0"),

           # 1.3. Date/delay effect - changes in value scaling (mu) ------------
           # - delay framing: mu = 1
           # - date framing: mu = mu_boost > 1

           date_delay_value_scaling = c("alpha", "mu1", "mu2", "beta", "kappa", "w", "a", "t0"),

           # 1.4. Defer/speedup effect - changes in time scaling (kappa) -------
           # - neutral framing: kappa = 1
           # - defer framing: kappa > 1 (over-responsive to deferrals)
           # - speedup framing: kappa < 1 (under-responsive to speedups)

           defer_speedup_time_scaling = c("alpha", "mu", "beta", "kappa1", "kappa2", "kappa3", "w", "a", "t0"),

           # 1.5. Defer/speedup effect - changes in value scaling (mu) ---------
           # - neutral framing:
           # - defer framing:
           # - speedup framing:

           defer_speedup_value_scaling = c("alpha", "mu1", "mu2", "beta", "kappa", "w", "a", "t0")
           )
       )[[model]][[parameterization]]

}

## get_par_bounds ##############################################################
#' Get parameter lower and upper bounds, given a model and parameterization
#'
#' These minimum and maximum values are based on reported values in the literature or best guesses, see code for details.
#'
#' @inheritParams fit_model
#' @param bound bound for which to obtain values, either "lower" or "upper"
#' @export
get_par_bounds = function(model = "DDM", parameterization = "", bound = "lower") {

  # 1. Define lower and upper bounds for all parameters ========================
  # Values are based on the following sources:
  # DB_JEPG_2014 - Dai & Busemeyer, J Exp Psychol Gen, 2014 - p. 1512
  # SR_JEPLMC_2013 - Scholten & Read, J Exp Psychol Learn Mem Cogn, 2013, p. 1197
  # rtdists - documentation accompanying the R package rtdists

  lowers = list('alpha' = .01, # DB_JEPG_2014
                'mu' = 1, # SR_JEPLMC_2013
                'mu_boost' = 1,
                'mu_loss' = 1, # SR_JEPLMC_2013
                'beta' = .01, # DB_JEPG_2014
                'kappa' = 1, # SR_JEPLMC_2013
                'kappa_contraction' = 0,
                'kappa_loss' = 1,
                'kappa_gain' = 0,
                'w' = 0.05, # DB_JEPG_2014
                "a" = 0.5, # rtdists
                "t0" = 0.05 # rtdists
                )

  uppers = list('alpha' = 2, # DB_JEPG_2014
                'mu' = 1, #
                'mu_boost' = 3,
                'mu_loss' = 3, # SR_JEPLMC_2013
                'beta' = 2, # DB_JEPG_2014
                'kappa' = 1, # SR_JEPLMC_2013
                'kappa_contraction' = 1, # guess
                'kappa_loss' = 3, # guess
                'kappa_gain' = 1,
                'w' = 0.95, # DB_JEPG_2014
                "a" = 2, # rtdists
                "t0" = 3 # DB_JEPG_2014 (data in Table 10)
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

                # 1.1. Changes in time scaling (kappa) -------------------------
                # - delay framing: kappa = 1
                # - date framing: kappa = kappa_contraction < 1

                date_delay_time_scaling =
                  list(lower = c(l['alpha'], l['mu'], l['beta'], l['kappa'], l['kappa_contraction'], l['w'], l['a'], l['t0']),
                       upper = c(u['alpha'], u['mu'], u['beta'], u['kappa'], u['kappa_contraction'], u['w'], u['a'], u['t0'])
                  ),

                # 1.2. Changes in value scaling (mu) ---------------------------
                # - delay framing: mu = 1
                # - date framing: mu = mu_boost > 1

                date_delay_value_scaling =
                  list(lower = c(l['alpha'], l['mu'], l['mu_boost'], l['beta'], l['kappa'], l['w'], l['a'], l['t0']),
                       upper = c(u['alpha'], u['mu'], u['mu_boost'], u['beta'], u['kappa'], u['w'], u['a'], u['t0'])
                  ),

                # 2. Defer-speedup effect ======================================

                # 2.1. Changes in time scaling (kappa) -------------------------
                # - neutral framing: kappa = 1
                # - defer framing: kappa > 1 (over-responsive to deferrals)
                # - speedup framing: kappa < 1 (under-responsive to speedups)

                defer_speedup_time_scaling =
                  list(lower = c(l['alpha'], l['mu'], l['beta'], l['kappa'], l['kappa_loss'], l['kappa_gain'], l['w'], l['a'], l['t0']),
                       upper = c(u['alpha'], u['mu'], u['beta'], u['kappa'], u['kappa_loss'], u['kappa_gain'], u['w'], u['a'], u['t0'])
                  ),

                # 2.2. Changes in value scaling (mu) ---------------------------
                # - neutral framing:
                # - defer framing:
                # - speedup framing:

                defer_speedup_value_scaling =
                  list(lower = c(l['alpha'], l['mu'], l['mu_loss'], l['beta'], l['kappa'], l['w'], l['a'], l['t0']),
                       upper = c(u['alpha'], u['mu'], u['mu_loss'], u['beta'], u['kappa'], u['w'], u['a'], u['t0'])
                  )
                )
         )[[model]][[parameterization]][[bound]]
    )
}


## get_par_values  #############################################################
#' Get parameter values
#'
#' @param x parameters
#' @inheritParams fit_model
#' @export
get_par_values = function(x, model = "DDM", parameterization = "") {

  # 1. Define parameter names ==================================================
  parameter_names <-
    get_par_names(model = model,
                  parameterization = "one_condition")
  params = list()
  names(x) = get_par_names(model = model,
                           parameterization = parameterization)

  # 2. Extract parameter values ================================================

  if (parameterization == "one_condition") {
    params = c(x["alpha"], x["mu"], x["beta"], x["kappa"], x["w"], x["a"], x["t0"])
    names(params) = parameter_names

  } else if (parameterization == "date_delay_time_scaling") {

    # 2.1. Date/delay effect - changes in time scaling (kappa) -----------------

    # delay frame: kappa = 1
    params[[1]] = c(x["alpha"], x["mu"], x["beta"], x["kappa1"], x["w"], x["a"], x["t0"])

    # date frame: kappa = kappa_contraction < 1
    params[[2]] = c(x["alpha"], x["mu"], x["beta"], x["kappa2"], x["w"], x["a"], x["t0"])

    names(params[[1]]) = names(params[[2]]) = parameter_names

  } else if (parameterization == "date_delay_value_scaling") {

    # 2.2. Date/delay effect - changes in value scaling (mu) -------------------

    # delay frame: mu = 1
    params[[1]] = c(x["alpha"], x["mu1"], x["beta"], x["kappa"], x["w"], x["a"], x["t0"])

    # date frame: mu = mu_boost > 1
    params[[2]] = c(x["alpha"], x["mu2"], x["beta"], x["kappa"], x["w"], x["a"], x["t0"])
    names(params[[1]]) = names(params[[2]]) = parameter_names

  } else if (parameterization == "defer_speedup_time_scaling") {

    # 2.3. Defer/speedup effect - changes in time scaling (kappa) --------------

    # neutral frame: kappa = 1
    params[[1]] = c(x["alpha"], x["mu"], x["beta"], x["kappa1"], x["w"], x["a"], x["t0"])

    # defer frame: kappa = 1
    params[[2]] = c(x["alpha"], x["mu"], x["beta"], x["kappa2"], x["w"], x["a"], x["t0"])

    # speedup frame: kappa = 1
    params[[3]] = c(x["alpha"], x["mu"], x["beta"], x["kappa3"], x["w"], x["a"], x["t0"])

    names(params[[1]]) = names(params[[2]]) = names(params[[3]]) = parameter_names
  } else if (parameterization == "defer_speedup_value_scaling") {

    # 2.4. Defer/speedup effect - changes in value scaling (mu) ----------------

    # neutral frame: mu = 1
    params[[1]] = c(x["alpha"], x["mu1"], x["beta"], x["kappa"], x["w"], x["a"], x["t0"])

    # defer frame: mu > 1 for
    params[[2]] = c(x["alpha"], x["mu2"], x["beta"], x["kappa"], x["w"], x["a"], x["t0"])

    # speedup frame: mu > 1 for
    params[[3]] = c(x["alpha"], x["mu2"], x["beta"], x["kappa"], x["w"], x["a"], x["t0"])

    names(params[[1]]) = names(params[[2]]) = names(params[[3]]) = parameter_names
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
                                                        s = 1,
                                                        a = unname(x['a']),
                                                        z = 0))
  } else if (sim_fun == 'rtdists_package') {
    purrr::map_df(.x = v,
                  .f = rtdists::rdiffusion,
                  n = n,
                  a = x['a'],
                  t0 = x['t0']
    ) %>%
      dplyr::mutate(v = v)
  }
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

## nest_join_stimuli_parameters ################################################
#' Nest and join stimulu and parameters
#'
#' nest_join_stimuli_parameters
#'
nest_join_stimuli_parameters <- function(stimuli = make_stimulus_df(),
                                         parameters, parameterization = "") {

  # Assertions -----------------------------------------------------------------
  assertthat::assert_that(parameterization %in% c("one_condition",
                                                  "date_delay_value_sensitivity",
                                                  "defer_speedup_value_scaling",
                                                  "date_delay_time_scaling",
                                                  "defer_speedup_time_scaling"
  ),
  msg = "Specify parameterization as \n\"one-condition\" (i.e., estimate parameters from a single experimental condition), \n\"date_delay_value_sensitivity\" (i.e., estimate free value function sensitivity parameter across two experimental conditions), \n\"date_delay_time_scaling\" (i.e., estimate free time function sensitivity parameter across two experimental conditions), \n\"defer_speedup_value_scaling\" (i.e., estimate free value function scaling parameter across two experimental conditions), or \n\"defer_speedup_time_scaling\" (i.e., estimate free time function scaling parameter  across two experimental conditions)."
  )

  # Repeat parameters, if stationary across conditions -------------------------
  if (parameterization %in% c("one-condition",
                              "date_delay_value_sensitivity",
                              "defer_speedup_value_scaling",
                              "date_delay_time_scaling",
                              "defer_speedup_time_scaling")
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
    tibble::as.tibble() %>%
    dplyr::mutate(frame = factor(levels(stimuli$frame),
                                 levels = levels(stimuli$frame))) %>%
    dplyr::select(frame, dplyr::everything()) %>%
    dplyr::group_by(frame) %>%
    tidyr::nest(.key = 'parameters')

  # Join nested stimuli and parameters -----------------------------------------
  nested_stimuli %>%
    dplyr::left_join(nested_parameters, by = "frame")

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
    # dplyr::mutate(observations = purrr::map2(.x = stimuli,
    #                                          .y = parameters,
    #                                          .f = itchmodel::itch_ddm),
    #               model = parameterization
    #               )

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

