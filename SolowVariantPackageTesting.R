# Install and load packages
# devtools::install_github("SebastianBehrens/SolowVariants", build_vignettes = T, force = T)
library(SolowVariants)
library(tidyverse)
library(R.utils)
theme_set(theme_bw())
# Run example code from vignette("ESSRL") =================================
# Setting up the parameter grid ---------------------------------
aux_np <- 200
aux_parameter_grid <-
  create_parameter_grid(
    c("alpha", "beta", "delta", "n", "s", "g", "X"),
    c(1/3, 1/3, 0.2, 0.005, 0.2, 0.02, 1),
    c(NA, NA, NA, NA, NA, NA, NA),
    c(NA, NA, NA, NA, NA, NA, NA),
    aux_np
  )

# Setting starting values for K and L ---------------------------------
aux_startvalues <- list(A = 1, K = 1, L = 1)

# Simulate the basic Solow growth model ---------------------------------
aux_simulation <- SimulateExtendedSolowModelScarceResourceLand(aux_parameter_grid, aux_np,aux_startvalues)

# Visualise some variables ---------------------------------
VisualiseSimulation(aux_simulation,
                    c("YpW", "KpW", "L"), 
                    "free")


# Issues =================================
# for seeing the code of the function at question
simulation_correctness_checker
?ESSRL_SS_CtO
# invoke error message
simulation_correctness_checker(aux_simulation, aux_parameter_grid, "ESSRL")

# interesting, because the functions seems to exist, but it is not callable with doCall.
# maybe wrong environment to the doCall function. but what is the environment in which the exists() call yields TRUE


simulation_correctness_checker
exists("docallissuetester")
docallissuetester()
MF_SS