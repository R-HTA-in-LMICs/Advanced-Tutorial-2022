# Misc settings -----------------------------------------------------------
pkgs <- c("BCEA", "dampack", "reshape2", "tidyverse", "darthtools")

# Install packages not yet installed:
installed_packages <- pkgs %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
 install.packages(pkgs[!installed_packages])
}

# Load required packages:
invisible(lapply(pkgs, library, character.only = TRUE))

# Model Settings ----------------------------------------------------------
# The following Markov model, coded in R, adapts the HIV/AIDS model found in 
# the book 'Decision Modelling in Health Economics' by Briggs et al.

# This section runs through the general the settings of the HIV/AIDS model as well 
# as creating the structure of the transition array and markov trace.

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
               dim = c(n_states, n_states, n_cycles),
               dimnames = list(
                v_names_states, v_names_states, 0:(n_cycles - 1)))
a_P_NT <- a_P_SoC

### Transition probabilities ------------------------------------------------
# This section enters the state specific transition probabilities for states A-D into
# the transition array.

#### Transitions for Status Quo ----------------------------------------------
## Transitions from health state A
a_P_SoC["A", "A", ] <- 0.721 # From A to A
a_P_SoC["A", "B", ] <- 0.202 # From A to B
a_P_SoC["A", "C", ] <- 0.067 # From A to C
a_P_SoC["A", "Death", ] <- 0.010 # From A to Death

## Transitions from health state B
a_P_SoC["B", "B", ] <- 0.581 # From B to B
a_P_SoC["B", "C", ] <- 0.407 # From B to C
a_P_SoC["B", "Death", ] <- 0.012 # From B to Death

## Transitions from health state C
a_P_SoC["C", "C", ] <- 0.750 # From C to C
a_P_SoC["C", "Death", ] <- 0.250 # From C to Death

# Transitions from health state Death
a_P_SoC["Death", "Death", ] <- 1 # From Death to Death

#### Transitions for New Treatment -------------------------------------------
# Note: for this comparator, all transitions in the first two years are dependent on
# the relative risk of moving to another state, i.e., conditional on being on the 
# New Treatment

# New Treatment effect risk ratio for two years of New Treatment
n_rr_trteffect <- 0.509 # treatment effect

## Transitions from health state A
a_P_NT["A", "A", ] <- a_P_SoC["A", "A", ] # from A to A
a_P_NT["A", "B", ] <- a_P_SoC["A", "B", ] # from A to B
a_P_NT["A", "C", ] <- a_P_SoC["A", "C", ] # from A to C
a_P_NT["A", "Death", ] <- a_P_SoC["A", "Death", ] # from A to Death
## Transitions from health state B
a_P_NT["B", "B", ] <- a_P_SoC["B", "B", ] # from B to B
a_P_NT["B", "C", ] <- a_P_SoC["B", "C", ] # from B to C
a_P_NT["B", "Death", ] <- a_P_SoC["B", "Death", ] # from B to Death

## Transitions from health state C
a_P_NT["C", "C", ] <- a_P_SoC["C", "C", ] # from C to C
a_P_NT["C", "Death", ] <- a_P_SoC["C", "Death", ] # from C to Death

# Transitions from health state Death
a_P_NT["Death", "Death", ] <- a_P_SoC["Death", "Death", ] # from Death to Death

##### Corrected transitions for first two years on New Treatment --------------
# Transitions from State A in first two years
a_P_NT["A", "B", 1:2] <- a_P_SoC["A", "B", 1:2] * n_rr_trteffect # from A to B
a_P_NT["A", "C", 1:2] <- a_P_SoC["A", "C", 1:2] * n_rr_trteffect # from A to C
a_P_NT["A", "Death", 1:2] <- a_P_SoC["A", "Death", 1:2] * n_rr_trteffect # from A to Death
a_P_NT["A", "A", 1:2] <- (1 - a_P_NT["A", "Death", 1:2]) * (1 - a_P_NT["A", "C", 1:2]) * (1 - a_P_NT["A", "B", 1:2]) # from A to A, using chain rule
# i.e., A to A is conditional on the joint distribution of not moving to other states etc. for B to B and C to C transitions

# Transitions from State B in first two years
a_P_NT["B", "C", 1:2] <- a_P_SoC["B", "C", 1:2] * n_rr_trteffect # from B to C
a_P_NT["B", "Death", 1:2] <- a_P_SoC["B", "Death", 1:2] * n_rr_trteffect # from B to Death
a_P_NT["B", "B", 1:2] <- (1 - a_P_NT["B", "C", 1:2]) * (1 - a_P_NT["B", "Death", 1:2]) # from B to B, using chain rule

# Transitions from State C in first two years
a_P_NT["C", "Death", 1:2] <- a_P_SoC["C", "Death", 1:2] * n_rr_trteffect # from C to Death
a_P_NT["C", "C", 1:2] <- 1 - a_P_NT["C", "Death", 1:2] # from C to C

# Model model -------------------------------------------------------------
# Create initial state vector for all health states at t = 0
v_s_init <- c("A" = 1, "B" = 0, "C" = 0, "Death" = 0) # initial state vector
# Initialize cohort trace for age-dependent cSTMs
m_M_SoC <- array(matrix(0, nrow = n_cycles + 1, ncol = n_states),
                dim = c(n_cycles + 1, n_states), 
                dimnames = list(0:n_cycles, v_names_states))
m_M_NT <- m_M_SoC
# Store the initial state vector in the first row of the cohort trace
m_M_SoC[1, ] <- v_s_init
m_M_NT[1, ] <- v_s_init

## Markov Trace ------------------------------------------------------------
# Iterative solution of age-dependent cSTM Status Quo
for (t in 1:n_cycles) {
  # Fill in cohort trace
  m_M_SoC[t + 1, ] <- m_M_SoC[t, ] %*% a_P_SoC[ , , t]
}
# Iterative solution of age-dependent cSTM New Treatment
for (t in 1:n_cycles) {
  # Fill in cohort trace
  m_M_NT[t + 1, ] <- m_M_NT[t, ] %*% a_P_NT[ , , t]
}

### Visualisation of Markov trace -------------------------------------------
# Associates each state with a colour:
cols <- c("A" = "#BD1B00", "B" = "#FA7700", "C" = "#FAD343", "Death" = "#B3BABA")
# Associates similar states with similar line types:
lty <-  c("A" = 1, "B" = 2, "C" = 3, "Death" = 4)

# Visualisation of cohort proportions for Status Quo:
ggplot(melt(apply(m_M_SoC, c(1, 2), mean)), aes(x = Var1, y = value, 
                      color = Var2, linetype = Var2)) +
 geom_line(size = 0.5) +
 scale_colour_manual(name = "Health state", 
                     values = cols) +
  scale_linetype_manual(name = "Health state",
                       values = lty) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Cycle") +
  ylab("Proportion of the cohort") +
  theme_light(base_size = 14) +
  theme(legend.position = "bottom", 
        legend.background = element_rect(fill = NA),
        text = element_text(size = 15))
# Survival curve for Markov Status Quo:
v_S_ad_1 <- 1 - m_M_SoC[, "Death"]  # vector with survival curve
ggplot(data.frame(Cycle = 0:n_cycles, Survival = v_S_ad_1), 
       aes(x = Cycle, y = Survival)) +
  geom_line(size = 1.3) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Cycle") +
  ylab("Proportion alive") +
  theme_bw(base_size = 14) +
  theme()

# Visualisation of cohort proportions for New Treatment:
ggplot(melt(apply(m_M_NT, c(1, 2), mean)), aes(x = Var1, y = value, 
                      color = Var2, linetype = Var2)) +
 geom_line(size = 0.5) +
 scale_colour_manual(name = "Health state", 
                     values = cols) +
  scale_linetype_manual(name = "Health state",
                       values = lty) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Cycle") +
  ylab("Proportion of the cohort") +
  theme_light(base_size = 14) +
  theme(legend.position = "bottom", 
        legend.background = element_rect(fill = NA),
        text = element_text(size = 15))
# Survival curve for Markov New Treatment:
v_S_ad_2 <- 1 - m_M_NT[, "Death"]  # vector with survival curve
ggplot(data.frame(Cycle = 0:n_cycles, Survival = v_S_ad_1), 
       aes(x = Cycle, y = Survival)) +
  geom_line(size = 1.3) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Cycle") +
  ylab("Proportion alive") +
  theme_bw(base_size = 14) +
  theme()

# Life expectancy for average individual in Markov model 1 cohort:
le_ad_1 <- sum(v_S_ad_1)
le_ad_1
# Life expectancy for average individual in Markov model  cohort:
le_ad_2 <- sum(v_S_ad_2)
le_ad_2

# to be continued