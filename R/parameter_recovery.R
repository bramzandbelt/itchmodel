parameter_recovery <- function(parameterization, frames, n_reps, output_dir) {

  library(itchmodel)
  library(tidyverse)

  # Preliminaries -----------------------------------------------------------

  ## Provide access to Magrittr's pipe
  "%>%" <- magrittr::`%>%`

  ## Set seed for random number generation
  set.seed(19821101)

  # Task parameters and stimuli (trials) ------------------------------------

  # Monetary amount (in Euro) of larger option
  m_l <- c(19.68)

  # Monetary amount (in Euro) of smaller option, experessed as percentage of larger option
  pct_m_l <- c(0.2, 0.5, 0.7, 0.8, 0.88, 0.93, 0.97, 0.99)

  # Delay of reward delivery of sooner option (in days)
  t_s <- c(0)

  # Intervals between sooner and later option (in days)
  interval <- c(1, 2, 3, 5, 7, 10, 14)

  # Generate stimuli (trials)
  stimuli <-
    itchmodel::make_stimulus_df(frames = frames,
                                m_l = m_l,
                                pct_m_l = pct_m_l,
                                t_s = t_s,
                                interval = interval,
                                n_reps = n_reps
    )

  # Optimization parameters for differential evolution algorithm ------------

  # Maximum number of iteration steps
  itermax <- 5

  # Relative convergence tolerance
  reltol <- 1e-6

  # Number of populations
  # NP_per_free_param <- 20

  # Model parameter values and bounds ---------------------------------------

  parameter_vals <- switch(parameterization,
                           date_delay_value_sensitivity = list('alpha' = c(0.85, 0.95), # first for delay, second for date frame
                                                               'mu' = 1,
                                                               'beta' = 0.70,
                                                               'kappa' = 1,
                                                               'w' = 0.5,
                                                               "a" = 1,
                                                               "t0" = 0.1
                                                               ),
                           date_delay_time_scaling = list('alpha' = 0.90,
                                                          'mu' = 1,
                                                          'beta' = 0.70,
                                                          'kappa' = c(1.2, 1), # first for delay, second for date frame
                                                          'w' = 0.5,
                                                          "a" = 1,
                                                          "t0" = 0.1
                                                          ),
                           defer_speedup_value_scaling = list('alpha' = 0.90,
                                                              'mu' = c(1, 1.2), # first is for neutral, second for defer and speedup frame
                                                              'beta' = 0.70,
                                                              'kappa' = 1,
                                                              'w' = 0.5,
                                                              "a" = 1,
                                                              "t0" = 0.1
                           ),
                           defer_speedup_time_scaling = list('alpha' = 0.90,
                                                             'mu' = 1,
                                                             'beta' = 0.70,
                                                             'kappa' = c(1, 1.2, 1/1.2), # first is for neutral, second for defer, third for speedup frame
                                                             'w' = 0.5,
                                                             "a" = 1,
                                                             "t0" = 0.1
                                                             )
                           )

  lowers <- get_par_bounds(model = "DDM",
                           parameterization = parameterization,
                           bound = "lower")
  uppers <- get_par_bounds(model = "DDM",
                           parameterization = parameterization,
                           bound = "upper")

  # Number of free parameters
  n_free <- sum(!(lowers == uppers))

  # Simulate data -----------------------------------------------------------

  synthetic_data <-
    itchmodel::sim_data(stimuli = stimuli,
                        parameters = parameter_vals,
                        parameterization = parameterization) %>%
    dplyr::select(-parameters)

  # Fit model to the data ---------------------------------------------------

  optim_data <-
    itchmodel::fit_model(data = synthetic_data,
                         model = "DDM",
                         parameterization = parameterization,
                         control = list(itermax = itermax,
                                        reltol = reltol
                                        # NP = NP_per_free_param * n_free
                                        )
                         )

  # Write the data to disk --------------------------------------------------

  file_name <- sprintf('parameter_recovery_%s_%dreps.Rdata',
                       parameterization,
                       n_reps
                       )

  save(optim_data,
       file = file.path(file.path(output_dir), file_name))
}









