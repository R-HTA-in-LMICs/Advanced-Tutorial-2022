# Model Settings ----------------------------------------------------------
# This section initiates the settings of the HIV/AIDS model
n_age_min <- 40 # age at baseline
n_age_max <- 60 # maximum age of follow up
n_cycles <- n_age_max - n_age_min # time horizon, number of cycles

# Define the names of the health states of the model:
v_names_states <- c("A", "B", "C", "Death")
n_states <- length(v_names_states) # record the number of health states

# define number of stochastic simulations
n_sims <- 1000

## Transition Array --------------------------------------------------------
# Transition array for each comparative intervention:
a_P_SoC <- array(0, 
               dim = c(n_states, n_states, n_cycles, n_sims),
               dimnames = list(
                v_names_states, v_names_states, 0:(n_cycles - 1), 1:n_sims))
a_P_NT <- a_P_SoC

