# Misc settings -----------------------------------------------------------
pkgs <- c("BCEA", "dampack", "reshape2", "tidyverse", "darthtools")

# Install packages if not installed
installed_packages <- pkgs %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
 install.packages(pkgs[!installed_packages])
}

# Load required packages
invisible(lapply(pkgs, library, character.only = TRUE))

# Model Settings ----------------------------------------------------------
# The following Markov model, coded in R, adapts the HIV/AIDS model found in 
# the book 'Decision Modelling in Health Economics' by Briggs et al.

# This section runs through the general the settings of the HIV/AIDS model and 
# defines the structure of the transition array and markov trace.

# Cohort settings
n_age_min <- 40 # age at baseline
n_age_max <- 60 # maximum age of follow up
n_cycles <- n_age_max - n_age_min # time horizon, number of cycles

# Define the names of the health states of the model
v_names_states <- c("A", "B", "C", "Death")
n_states <- length(v_names_states) # record the number of health states

# define number of stochastic simulations
# n_sims <- 1000

## Transition Array --------------------------------------------------------
# Transition array for each comparative intervention
a_P_SoC <- array(0, 
               dim = c(n_states, n_states, n_cycles),
               dimnames = list(
                v_names_states, v_names_states, 0:(n_cycles - 1)))
a_P_NT <- a_P_SoC

### Transition probabilities ------------------------------------------------
# This section enters the state specific transition probabilities for states A-D into
# the transition array.

## Transitions from health state A
p_AA <- 0.721 # transition from health state A to A
p_AB <- 0.202 # transition from health states A to B
p_AC <- 0.067 # transition from health states A to C
p_AD <- 0.010 # transition from health states A to Death

## Transitions from health state B
p_BB <- 0.581 # transition from health states B to B
p_BC <- 0.407 # transition from health states B to C
p_BD <- 0.012 # transition from health states B to Death

## Transitions from health state C
p_CC <- 0.750 # transition from health states C to C
p_CD <- 0.250 # transition from health states C to Death

# Transitions from health state Death
p_DD <- 1 # transition from health states Death to Death

# New Treatment effect risk ratio for two years of New Treatment
n_rr_trteffect <- 0.509 # treatment effect

#### Transitions for Status Quo ----------------------------------------------
# Transitions from health state A
a_P_SoC["A", "A", ] <- p_AA # transition from health state A to A
a_P_SoC["A", "B", ] <- p_AB # transition from health state A to B
a_P_SoC["A", "C", ] <- p_AC # transition from health state A to C
a_P_SoC["A", "Death", ] <- p_AD # transition from health state A to Death

# Transitions from health state B
a_P_SoC["B", "B", ] <- p_BB # transition from health state B to B
a_P_SoC["B", "C", ] <- p_BC # transition from health state B to C
a_P_SoC["B", "Death", ] <- p_BD # transition from health state B to Death

# Transitions from health state C
a_P_SoC["C", "C", ] <- p_CC # transition from health state C to C
a_P_SoC["C", "Death", ] <- p_CD # transition from health state C to Death

# Transitions from health state Death
a_P_SoC["Death", "Death", ] <- p_DD # transition from health state Death to Death

#### Transitions for New Treatment -------------------------------------------
# Note: for this comparator, all transitions in the first two years are dependent on
# the relative risk of moving to another state, i.e., conditional on being on New 
# Treatment. Hence, duplicate transitions for all years, and then correct first two
# cycles.
a_P_NT <- a_P_SoC

##### Corrected transitions for first two years on New Treatment --------------
# Transitions from State A in first two years
a_P_NT["A", "B", 1:2] <- p_AB * n_rr_trteffect # transition from health state from A to B
a_P_NT["A", "C", 1:2] <- p_AC * n_rr_trteffect # transition from health state from A to C
a_P_NT["A", "Death", 1:2] <- p_AD * n_rr_trteffect # transition from health state from A to Death
a_P_NT["A", "A", 1:2] <- (1 - p_AB * n_rr_trteffect) * (1 - p_AC * n_rr_trteffect) * (1 - p_AD * n_rr_trteffect) # transition from health state from A to A, using chain rule
# i.e., A to A is conditional on the joint distribution of not moving to other, 
# worse states and etc. for B to B and C to C transitions

# Transitions from State B in first two years
a_P_NT["B", "C", 1:2] <- p_BC * n_rr_trteffect # transition from health state from B to C
a_P_NT["B", "Death", 1:2] <- p_BD * n_rr_trteffect # transition from health state from B to Death
a_P_NT["B", "B", 1:2] <- (1 - p_BC * n_rr_trteffect) * (1 - p_BD * n_rr_trteffect) # transition from health state from B to B, using chain rule

# Transitions from State C in first two years
a_P_NT["C", "Death", 1:2] <- p_CD * n_rr_trteffect # transition from health state from C to Death
a_P_NT["C", "C", 1:2] <- 1 - p_CD * n_rr_trteffect # transition from health state from C to C

# Markov model -------------------------------------------------------------
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
for (t in 1:n_cycles) {
  # Iterative solution of cSTM Status Quo
  m_M_SoC[t + 1, ] <- m_M_SoC[t, ] %*% a_P_SoC[ , , t]
  # Iterative solution of cSTM New Treatment
  m_M_NT[t + 1, ] <- m_M_NT[t, ] %*% a_P_NT[ , , t]
}

### Visualisation of Markov trace -------------------------------------------
# Associates each state with a colour
cols <- c("A" = "#FF0000", "B" = "#FF7400", "C" = "black", "Death" = "#00B6FF")
# Associates health states with specific line types
lty <-  c("A" = 1, "B" = 2, "C" = 3, "Death" = 4)

# Visualisation of cohort proportions for Status Quo
ggplot(melt(apply(m_M_SoC, c(1, 2), mean)), aes(x = Var1, y = value, 
                      color = Var2, linetype = Var2)) +
 geom_line(size = 0.5) +
 ggtitle("Standard of Care") +
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
# Survival curve for Markov Status Quo
v_S_ad_1 <- 1 - m_M_SoC[, "Death"]  # vector with survival curve
ggplot(data.frame(Cycle = 0:n_cycles, Survival = v_S_ad_1), 
       aes(x = Cycle, y = Survival)) +
  geom_line(size = 1.3) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Cycle") +
  ylab("Proportion alive") +
  theme_bw(base_size = 14) +
  theme()

# Visualisation of cohort proportions for New Treatment
ggplot(melt(apply(m_M_NT, c(1, 2), mean)), aes(x = Var1, y = value, 
                      color = Var2, linetype = Var2)) +
 geom_line(size = 0.5) +
 ggtitle("New Treatment") +
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
# Survival curve for Markov New Treatment
v_S_ad_2 <- 1 - m_M_NT[, "Death"]  # vector with survival curve
ggplot(data.frame(Cycle = 0:n_cycles, Survival = v_S_ad_1), 
       aes(x = Cycle, y = Survival)) +
  geom_line(size = 1.3) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Cycle") +
  ylab("Proportion alive") +
  theme_bw(base_size = 14) +
  theme()

# Life expectancy for average individual in Markov model 1 cohort
le_ad_1 <- sum(v_S_ad_1)
le_ad_1
# Life expectancy for average individual in Markov model cohort
le_ad_2 <- sum(v_S_ad_2)
le_ad_2

# Economic Analysis -------------------------------------------------------
# Initialize transition-dynamics array under SoC and New Treatment
a_A_SoC <- array(0,
             dim = c(n_states, n_states, (n_cycles + 1)),
             dimnames = list(v_names_states, v_names_states, 0:n_cycles))
# New Treatment
a_A_NT <- a_A_SoC

# Set first slice to the initial state vector in its diagonal
diag(a_A_SoC[, , 1]) <- v_s_init
diag(a_A_NT[, , 1]) <- v_s_init

for (t in 1:n_cycles){
 # Iterative solution to produce the transition-dynamics array under SoC
 a_A_SoC[, , t + 1] <- diag(m_M_SoC[t, ]) %*% a_P_SoC[ , , t]
 # Iterative solution to produce the transition-dynamics array under New Treatment
 a_A_NT[, , t + 1] <- diag(m_M_NT[t, ]) %*% a_P_NT[ , , t]
}

### Costs -------------------------------------------------------------------
# Drug costs
c_AZT <- 2278 # Zidovudine drug cost (monotheray)
c_Lam <- 2086.50 # Lamivudine drug cost (added therapy)

# Direct health state costs
c_direct_state_A <- 1701 # direct costs associated with health state A
c_direct_state_B <- 1774 # direct costs associated with health state B
c_direct_state_C <- 6948 # direct costs associated with health state C
# Indirect health state costs
c_indirect_state_A <- 1055 # indirect costs associated with health state A
c_indirect_state_B <- 1278 # indirect costs associated with health state B
c_indirect_state_C <- 2059 # indirect costs associated with health state C

# Vector of costs:
v_c_SoC <- c(c_direct_state_A + c_indirect_state_A, 
             c_direct_state_B + c_indirect_state_B, 
             c_direct_state_C + c_indirect_state_C, 
             0)
# Array of state costs for Standard of Care
a_c_SoC <- array(matrix(v_c_SoC, nrow = n_states, ncol = n_states, byrow = T),
                  dim = c(n_states, n_states, n_cycles + 1),
                  dimnames = list(v_names_states, v_names_states, 0:n_cycles))
# Note: the advantage of creating a cost array with the same dimensions as the 
# transition array is that you can take advantage of vectorisation - which is very
# fast in R.

# Add drug costs
a_c_SoC[ , c("A", "B", "C"), ] <- a_c_SoC[ , c("A", "B", "C"), ] + c_AZT  # drug costs for SoC
a_c_NT <- a_c_SoC # create New Treatment costs array 
a_c_NT[ , c("A", "B", "C"), 1:2] <- a_c_NT[ , c("A", "B", "C"), 1:2] + c_Lam # drug costs for NT (combination therapy for first two cycles, mono therafter)

# Total costs
## Standard of Care
a_Y_c_SoC <- a_A_SoC * a_c_SoC
## Standard of Care
a_Y_c_NT <- a_A_NT * a_c_NT

# Calculate total costs per cycle
m_costs_SoC <- rowSums(t(colSums(a_Y_c_SoC))) # SoC
m_costs_NT <- rowSums(t(colSums(a_Y_c_NT))) # New Treatment

### Life Years --------------------------------------------------------------
# Standard of Care
m_lys_SoC <- rowSums(t(colSums(a_A_SoC[, c("A", "B", "C"), ]))) # LYs per cycle
sum(rowSums(t(colSums(a_A_SoC[, c("A", "B", "C"), ])))) # Total LYs

# New Treatment
m_lys_NT <- rowSums(t(colSums(a_A_NT[, c("A", "B", "C"), ]))) # LYs per cycle
sum(rowSums(t(colSums(a_A_NT[, c("A", "B", "C"), ])))) # Total LYs

### Discounting -------------------------------------------------------------
# Discount rates
d_e <- 0.0
d_c <- 0.06
# Discount weights for costs
v_dwc <- 1 / ((1 + d_c) ^ (0:n_cycles))
# Discount weights for effects
v_dwe <- 1 / ((1 + d_e) ^ (0:n_cycles))

# Apply discount
v_lys_disc_SoC <- t(m_lys_SoC) %*% v_dwe # SoC QALYs
v_costs_disc_SoC <- t(m_costs_SoC) %*% v_dwc # SoC costs

v_lys_disc_NT <- t(m_lys_NT) %*% v_dwe # NT QALYs
v_costs_disc_NT <- t(m_costs_NT) %*% v_dwc # NT costs

## Cost-effectiveness ------------------------------------------------------
icer <- (v_costs_disc_NT - v_costs_disc_SoC) / (v_lys_disc_NT - v_lys_disc_SoC) # deterministic icer
icer # where NT is the reference treatment

# End of file -------------------------------------------------------------