#' Intertemporal choice task staircase procedure
#'
#' @param m_ll Monetary amount of large-but-later option
#' @param t_ss Time of reward delivery of small-but-sooner option
#' @param t_lls Vector of times of reward delivery of large-but-later option
#' @param n_rep Number of repetitions
#' @param procedure Type of staircase procedure
#' @param model Model class (currently, only "DDM" supported)
#' @param parameterization Model parameterization
#' @param parameters List of parameter values
#' @param step_size_decreasing Whether or not staircase step size should decrease over time
#' @param n_trial Number of trials per t_ll
#' @export
staircase_procedure <- function(m_ll = 20, t_ss = 0, t_lls = c(1,2,5,7,12,14),
                                n_rep = 1, procedure = '1up1down', model = "DDM",
                                parameterization = "",
                                parameters, step_size_decreasing = TRUE, decrease_every_trial = TRUE, n_trial = 10, step_size_fixed = 0.005) {

  ## 1. Preliminaries ##########################################################

  x <- unlist(parameters)

  print(x)

  names(x) <- itchmodel::get_par_names(model = model,
                                       parameterization = parameterization)


  # Determine the number of trials per delay (excluding catch trials) ----------
  # More trials are needed with nondecreasing step size
  # if (step_size_decreasing) {
  #   n_trial <- 10
  # } else {
  #   n_trial <- 50
  # }

  # Initiate data frame for logging responses ----------------------------------
  responses <-
    expand.grid(m_ss = NA,
                true_i_p = NA,
                p0707 = NA,
                p0293 = NA,
                adjustment_factor = NA,
                step_size = NA,
                m_ll = m_ll,
                t_ss = t_ss,
                t_ll = t_lls,
                trial = seq(n_trial + 2),
                rep = seq(n_rep),
                v = NA,
                p_ll = NA,
                choice = NA_integer_)

  ## 2. Staircase procedure ####################################################

  # Loop over repetitions ======================================================
  for (i_rep in 1:n_rep) {

    # Loop over delays =========================================================
    for (i_t_ll in 1:length(t_lls)) {

      # Set delay to later reward
      t_ll <- t_lls[i_t_ll]

      print(sprintf('parameterization: %s, rep: %d, t_ll: %d', parameterization, i_rep, t_ll))

      # Initiate vectors for logging trial-to-trial variation in choice, m_ss, step_size, and adjustment_factor
      choice <-
        m_ss <-
        step_size <-
        adjustment_factor <-
        v <-
        p_ll <-
        rep(NA, n_trial + 2)

      if (step_size_decreasing & !decrease_every_trial) {
        n_decrease <- 0
        step_size_reversal <- m_ll * 0.1
        counter <- 0
      }

      # Compute true indifference point
      base_element <- unname(m_ll^x['alpha'] - ((1 - x['w']) * (t_ll^x['beta'] - t_ss^x['beta'])) / x['w'])
      power_coefficient <- unname(1 / x['alpha'])
      true_i_p <- rep(base_element ^ power_coefficient, n_trial + 2)

      # Approximation of m_ss for when P(LL) equals sqrt(2)/2 and (1 - sqrt(2)/2)
        # Assumes z = 0 and s = 1

      if (procedure == '2up1down' | procedure == '1up2down') {
        tmp1 <- unname(m_ll^x['alpha'] - ((1 - x['w']) * (t_ll^x['beta'] - t_ss^x['beta']) + -0.8815 / (-2 * 2 * x['a'])) / x['w'])
        tmp2 <- unname(m_ll^x['alpha'] - ((1 - x['w']) * (t_ll^x['beta'] - t_ss^x['beta']) + 0.8815 / (-2 * 2 * x['a'])) / x['w'])
        p0707 <- rep(unname(exp(log(tmp1) / x['alpha'])), n_trial + 2)
        p0293 <- rep(unname(exp(log(tmp2) / x['alpha'])), n_trial + 2)
      } else {
        p0707 <- NA
        p0293 <- NA
      }

      # Loop over trials =======================================================
      for (i_trial in 1:(n_trial + 2)) {

        # 1. Determine m_ss ----------------------------------------------------
        if (i_trial == 1)
          # Catch trial 1: choose between \euro 0 now and \euro m_ll later
          {
          m_ss[i_trial] <- 0
          }
        else if (i_trial == 2)
          # Catch trial 2: choose between \euro m_ll now and \euro m_ll later
          {
          m_ss[i_trial] <- m_ll
        }
        else if (i_trial == 3)
          # First trial of staircase procedure: start at half m_ss
          {
          m_ss[i_trial] <- 0.5 * m_ll
        }
        else if (i_trial > 3)
        {
          m_ss[i_trial] <- m_ss[i_trial - 1] + adjustment_factor[i_trial] * step_size[i_trial]
        }

        # 2. Get drift rate and choice -----------------------------------------
        params <- c(x["alpha"], x["beta"], x["w"], x["a"], x["t0"])
        params['mu'] <- 1
        params['kappa'] <- 1

        v[i_trial] <-
          itchmodel::compute_drift_rate(parameters = params,
                                        stimuli = itchmodel::make_stimulus_df(m_l = m_ll,
                                                                              pct_m_l = m_ss[i_trial] / m_ll,
                                                                              t_s = t_ss,
                                                                              interval = t_ll - t_ss,
                                                                              n_reps = 1),
                                        parameterization = "one_condition",
                                        frame = "neutral")

        p_ll[i_trial] <-
          itchmodel::db_bin_choice_prob(d = v[i_trial],
                                        s = 1,
                                        a = parameters$a,
                                        z = 0)

        choice[i_trial] <-
          rbinom(n = 1,
                 size = 1,
                 prob = p_ll[i_trial]
        )


        # 3. Determine what adjustments are need, if any, on the next trial ----

        if (procedure == '1up1down')
          # The 1-up-1-down procedure approximates P(LL) = 0.5
          {
          if (i_trial == 2)
            {
            adjustment_factor[i_trial + 1] <- -1
            if (step_size_decreasing) {
              if (decrease_every_trial) {
                step_size[i_trial + 1] <- 2^-(1) * m_ll
              } else {
                step_size[i_trial + 1] <- step_size_reversal
              }
            } else {
              step_size[i_trial + 1] <- step_size_fixed
            }
          }
          else if (i_trial >= 3 & i_trial < (n_trial + 2))
            {

            # Adjustment factor
            if (choice[i_trial] == 1)
            # If LL chosen on this trial, then the SS offer will be increased on the next trial
            {adjustment_factor[i_trial + 1] <- 1
            } else
            # If SS chosen on this trial, then the SS offer will be decreased on the next trial
            {adjustment_factor[i_trial + 1] <- -1
            }

            # Step size
            if (step_size_decreasing) {
              if (decrease_every_trial) {
                step_size[i_trial + 1] <- 2^-(i_trial - 1) * m_ll
              } else {
                if (i_trial >= 7) {
                  # If at least five non-catch trials were presented
                  if (sum(abs(diff(choice[(i_trial - 4):i_trial]))) > 1 & counter >= 5) {
                    # If at least two reversals happened in the previous 5 trials, then decrease step size
                    step_size_reversal <- step_size_reversal / 2
                    counter <- 0
                  }
                }
                step_size[i_trial + 1] <- step_size_reversal
                counter <- counter + 1
              }
            } else
              step_size[i_trial + 1] <- m_ll * step_size_fixed
            }
          }
        else if (procedure == '2up1down')
          # The 2-up-1-down procedure approximates P(LL) = 0.707
          {
          if (i_trial == 2 | i_trial == 3 )
            # Only set adjustment factor
            {
            adjustment_factor[i_trial + 1] <- -1

            if (step_size_decreasing) {
              if (decrease_every_trial) {
                step_size[i_trial + 1] <- 2^-(1) * m_ll
              } else {
                step_size[i_trial + 1] <- step_size_reversal
              }
            } else {
              step_size[i_trial + 1] <- step_size_fixed
            }

            }
          else if (i_trial >= 4 & i_trial < (n_trial + 2)) {

            if (all(choice[(i_trial - 1):i_trial] == c(1,1))) {
              # If LL chosen on this and previous trial, then increase SS offer on the next trial
              adjustment_factor[i_trial + 1] <- 1
            } else {
              # If SS chosen on this or previous trial, then decrease SS offer on the next trial
              adjustment_factor[i_trial + 1] <- -1
            }
          }

          # Step size
          if (step_size_decreasing) {
            if (decrease_every_trial) {
              step_size[i_trial + 1] <- 2^-(i_trial - 1) * m_ll
            } else {
              if (i_trial >= 7) {
                # If at least five non-catch trials were presented
                if (sum(abs(diff(choice[(i_trial - 4):i_trial]))) > 1 & counter >= 5) {
                  # If at least two reversals happened in the previous 5 trials, then decrease step size
                  step_size_reversal <- step_size_reversal / 2
                  counter <- 0
                }
              }
              step_size[i_trial + 1] <- step_size_reversal
              counter <- counter + 1
            }
          } else
            step_size[i_trial + 1] <- m_ll * step_size_fixed
        }

        else if (procedure == '2down1up')
          # The 2-down-1-up procedure approximates P(LL) = 0.293
          {

          if (i_trial == 2 | i_trial == 3 ) {
            adjustment_factor[i_trial + 1] <- 1

            if (step_size_decreasing) {
              if (decrease_every_trial) {
                step_size[i_trial + 1] <- 2^-(1) * m_ll
              } else {
                step_size[i_trial + 1] <- step_size_reversal
              }
            } else {
              step_size[i_trial + 1] <- step_size_fixed
            }

          }
          else if (i_trial >= 4 & i_trial < (n_trial + 2)) {

            if (all(choice[(i_trial - 1):i_trial] == c(0,0))) {
              # If LL chosen on this and previous trial, then increase SS offer on the next trial
              adjustment_factor[i_trial + 1] <- -1
            } else {
              # If SS chosen on this or previous trial, then decrease SS offer on the next trial
              adjustment_factor[i_trial + 1] <- 1
            }

          }

          # Step size
          if (step_size_decreasing) {
            if (decrease_every_trial) {
              step_size[i_trial + 1] <- 2^-(i_trial - 1) * m_ll
            } else {
              if (i_trial >= 7) {
                # If at least five non-catch trials were presented
                if (sum(abs(diff(choice[(i_trial - 4):i_trial]))) > 1 & counter >= 5) {
                  # If at least two reversals happened in the previous 5 trials, then decrease step size
                  step_size_reversal <- step_size_reversal / 2
                  counter <- 0
                }
              }
              step_size[i_trial + 1] <- step_size_reversal
              counter <- counter + 1
            }
          } else {
            step_size[i_trial + 1] <- m_ll * step_size_fixed
          }

        }

      } # end of trials loop

      # Log choices etc. -------------------------------------------------------
      i_rows <- (responses$t_ll == t_ll) & (responses$rep == i_rep)

      responses$m_ss[i_rows] <- m_ss
      responses$step_size[i_rows] <- step_size
      responses$adjustment_factor[i_rows] <- adjustment_factor
      responses$p_ll[i_rows] <- p_ll
      responses$v[i_rows] <- v
      responses$true_i_p[i_rows] <- true_i_p
      responses$p0707[i_rows] <- p0707
      responses$p0293[i_rows] <- p0293
      responses$choice[i_rows] <- choice

    } # end of delays loop
  } # end of repetition loop

  # Output
  responses %>%
    dplyr::arrange(t_ll, rep, trial)

} # end of function

#' Define experimental trials based on indifference point staircase procedure data
#'
#' @param ip_data Indifference point procedure data frame
#' @export n_staircase_trials Number of staircase procedure trials
define_expt_trials <- function(ip_data, parameterization, n_staircase_trials, n_reps) {
  ip_data %>%
    dplyr::filter(trial == max(trial)) %>%
    dplyr::select(t_ll, m_ss, true_i_p) %>%
    dplyr::rename(around = m_ss) %>%
    # Default difference between around and below/above is m_ll * 2^-(n_staircase_trials - 2) (€0.68)
    # However, the staircase procedure approaches 0/m_ll by 2^-(n_staircase_trials) and we want to prevent that below will be smaller than 0 and greater than m_ll. The below procedure makes sure this happens, by (asymmetrically) decreasing the difference between around and below/above until conditions are satisfied.
    dplyr::mutate(below = ifelse(around - m_ll * 2^-(n_staircase_trials - 2) > 0,
                                 around - m_ll * 2^-(n_staircase_trials - 2),
                                 ifelse(around - m_ll * 2^-(n_staircase_trials - 1) > 0,
                                        around - m_ll * 2^-(n_staircase_trials - 1),
                                        ifelse(around - m_ll * 2^-(n_staircase_trials) > 0,
                                               around - m_ll * 2^-(n_staircase_trials),
                                               ifelse(around - m_ll * 2^-(n_staircase_trials + 1) > 0,
                                                      around - m_ll * 2^-(n_staircase_trials + 1),
                                                      NA
                                                      )
                                               )
                                        )
                                 ),
                  above = ifelse(around + m_ll * 2^-(n_staircase_trials - 2) < m_ll,
                                 around + m_ll * 2^-(n_staircase_trials - 2),
                                 ifelse(around + m_ll * 2^-(n_staircase_trials - 1) < m_ll,
                                        around + m_ll * 2^-(n_staircase_trials - 1),
                                        ifelse(around + m_ll * 2^-(n_staircase_trials) < m_ll,
                                               around + m_ll * 2^-(n_staircase_trials),
                                               ifelse(around + m_ll * 2^-(n_staircase_trials + 1) < m_ll,
                                                      around + m_ll * 2^-(n_staircase_trials + 1),
                                                      NA
                                               )
                                        )
                                 )
                  )
    ) %>%
    tidyr::gather(key = "m_s_cat", value = "m_ss", below, around, above) %>%
    dplyr::arrange(t_ll, m_ss) %>%
    dplyr::rename(t_l = t_ll,
                  m_s = m_ss) %>%
    tidyr::crossing(frame = factor(itchmodel::get_frames(parameterization),
                                   levels = itchmodel::get_frames(parameterization)),
                    rep = 1:n_reps
    ) %>%
    dplyr::mutate(m_l = m_ll,
                  t_s = t_ss) %>%
    dplyr::select(frame, m_s, t_s, m_l, t_l, m_s_cat)

}

#' Plot indifference point procedure
#'
#' @param df Tibble with data from indifference point procedure
#' @export
plot_my_indifference_point_procedure <- function(df) {
  dummy_data <-
    df %>%
    dplyr::filter(trial == 1 & rep == 1) %>%
    dplyr::select(true_i_p, p0293, p0707, t_ll, m_ll)

  plt <-
    ggplot2::ggplot(df,
                    ggplot2::aes(x = trial,
                                 y = m_ss,
                                 group = rep)) +
    ggplot2::facet_wrap('t_ll', ncol = 1)

  plt <-
    plt +
    ggplot2::geom_hline(data = dummy_data,
                        ggplot2::aes(yintercept = true_i_p),
                        color = 'red') +

    ggplot2::geom_hline(data = dummy_data,
                        ggplot2::aes(yintercept = p0293),
                        linestyle = "dashed",
                        color = 'red') +

    ggplot2::geom_hline(data = dummy_data,
                        ggplot2::aes(yintercept = p0707),
                        linestyle = "dashed",
                        color = 'red')

  plt <-
    plt +
    ggplot2::geom_step()

  if (length(unique(df$rep)) > 1) {
    plt <-
      plt +
      ggplot2::geom_step(data =
                           df %>%
                           dplyr::group_by(t_ll, trial) %>%
                           dplyr::select(t_ll, trial, m_ss, rep) %>%
                           dplyr::summarize_all(mean),
                         ggplot2::aes(x = trial,
                                      y = m_ss),
                         size = 1.5
      )
  }



  plt <-
    plt +
    ggplot2::scale_x_continuous(name = "Trial",
                                breaks = seq(2,10,2)) +
    ggplot2::scale_y_continuous(name = parse(text = "M[SS] (Euro)"),
                                limits = c(0,max(dummy_data$m_ll))) +
    ggplot2::theme_minimal()

  plt
}

estimate_auc <- function() {

  bla <-
    synthetic_data %>%
    dplyr::select(-model) %>%
    dplyr::filter(frame == "neutral") %>%
    tidyr::unnest() %>%
    dplyr::group_by(t_l) %>%
    tidyr::nest()

  df <-
    bla$data[[1]] %>%
    dplyr::select(m_s, response)



}
