### 1.0 Extended Solow Growth Model for Endogeneous Growth #############################

SimulateExtendedSolowModelEndogenousGrowth <- function(paragrid, np, startvals){

  # Roxygen Header ---------------------------------
  #' @title Simulates the ESEG Solow variant
  #' @description Simulates all (both primary and secondary) endogenous variables to the extended Solow growth model with endogenous technological growth.
  #' @inheritParams SimulateBasicSolowModel
  #' @note The structural equations to this model can be found in the vignette to this package:
  #' \code{vignette("SolowVariants")}
  #' @export

  # Function ---------------------------------

    # Initialize Simulation Table ---------------------------------
    sim_table <- create_simulation_table(variable_encoder(meta_ESEG_variables), np)
    
    # Fill Start Values for Period 0 ---------------------------------
    aux_index <- which(sim_table$period == 0)
    sim_table[[aux_index, "L"]] <- startvals$L
    sim_table[[aux_index, "K"]] <- startvals$K
    sim_table[[aux_index, "Y"]] <- ESEG_MF_Y(
      sim_table[["K"]][[which(sim_table$period == 0)]],
      sim_table[["L"]][[which(sim_table$period == 0)]],
      paragrid[["alpha"]][[which(paragrid$period == 0)]],
      paragrid[["phi"]][[which(paragrid$period == 0)]],
      sim_table[["L"]][[which(sim_table$period == 0)]]) # dirty workaround on last function input
    
    # Computing Variables after Period 0 ---------------------------------
    for (i in 1:np){
        # i <- 1
        # print(i)
        aux_index <- which(sim_table$period == i)
        sim_table[[aux_index, "L"]] <- ESEG_MF_LN(
          paragrid[["n"]][[which(paragrid$period == i-1)]],
          sim_table[["L"]][[which(sim_table$period == i-1)]])
        sim_table[[aux_index, "K"]] <- ESEG_MF_KN(
          paragrid[["s"]][[which(paragrid$period == i-1)]],
          sim_table[["Y"]][[which(sim_table$period == i-1)]],
          paragrid[["delta"]][[which(paragrid$period == i-1)]],
          sim_table[["K"]][[which(sim_table$period == i-1)]])
        sim_table[[aux_index, "Y"]] <- ESEG_MF_Y(
          sim_table[["K"]][[aux_index]],
          sim_table[["L"]][[aux_index]],
          paragrid[["alpha"]][[aux_index]],
          paragrid[["phi"]][[aux_index]],
          sim_table[["L"]][[aux_index]]) # dirty workaround on last function input
        # explanation on dirty workaround
        # ESEG_MF_Y computes differently depending on phi (semi or fully endo growth)
        # since TFP is computed as an additional variable (with add_var_computer) it is not available in the simulation part itself. 
        # in other words TFP is a secondary endo variable here.
        # TFP is however used in the ESEG_MF_Y when phi is close to 1 (case of fully endo growth)
        # yet TFP is not available!
        # but since TFP is defined as a constant based on L (a constant, since no grwoth in L in fully endo growth setting which takes n as 0)
        # ...., we can use L (which is 1) as essentially containing the value of TFP (which will be filled in later and is L^(1-alpha) and 1^k k in R is 1)
    }
    
    # Remark on n = 0 in case of fully endogenous growth (phi -> 1) ---------------------------------
    if(any(unique(paragrid[["phi"]]) %>% between (0.95, 1))){
        if(any(unique(paragrid[["n"]])!=0)){
        warning("Given the entered value for phi is close to 1, which approximates the fully endogenous ESEG, the parameter n should be set at 0.")
    }

    }
    # Computing Additional Variables ---------------------------------
    
    remaining_vars_to_compute_bool <- names(sim_table) %in% c("period", "L", "K", "Y")
    
    sim_table <- add_var_computer(sim_table, remaining_vars_to_compute_bool, paragrid, "special", "ESEG")
    
    # Remark on Stability Conditions ---------------------------------
    phi <- paragrid[["phi"]][[dim(paragrid)[[1]]]]
    s <- paragrid[["s"]][[dim(paragrid)[[1]]]]
    A <- sim_table[["TFP"]][[dim(sim_table)[[1]]]]
    n <- paragrid[["n"]][[dim(paragrid)[[1]]]]
    delta <- paragrid[["delta"]][[dim(paragrid)[[1]]]]




    if(phi < 0.95){
            if(((1 + n)^(1/(1-phi))) > (1 - delta)){
                warning("Stability condition of the semi endogeneous ESEG (((1 + n)^(1/(1-phi))) > (1 - delta)) is fulfilled.")
            }else{
              warning("Stability condition of the semi endogeneous ESEG (((1 + n)^(1/(1-phi))) > (1 - delta)) is NOT fulfilled.")
            }
    }else if(phi >= 0.95 && phi< 1){
            if(s * A - delta > 0){
                warning("Stability condition of the fully endogeneous ESEG (s*A - delta > 0) is fulfilled.")
            }else{
              warning("Stability condition of the fully endogeneous ESEG (s*A - delta > 0) is NOT fulfilled.")
            }
    }else if(phi > 1){
            NaN
    }
    return(sim_table)
}
# Working for phi << 1 but not for phi ~ 1 (not sure why)

# # Testing
# phi <- 0.9999
# n <- 0
# delta <- 0.15
# s <- 0.2
# 
# 
# testnamel <- c("alpha", "phi", "delta", "n", "s")
# testivl <- c(1/3, phi, delta, n, s)
# testpfcl <- c(NA,NA,NA, NA, NA)
# testnvl <- c(NA, NA, NA, NA, NA)
# np <- 200
# testgridalt <- create_parameter_grid(testnamel, testivl, testpfcl, testnvl, np)
# paragrid <- testgridalt
# startvals <- list(K = 1, L = 1)
# testsimulation <- SimulateExtendedSolowModelEndogenousGrowth(testgridalt, np,startvals)
# 
# source("HelperFunctions.R")
# 
# (1 + n)^(1/(1- phi)) > 1- delta
# (testsimulation[["TFP"]][[201]] - testsimulation[["TFP"]][[200]])/testsimulation[["TFP"]][[200]]
# ESEG_SS_gYpW(n, phi, s, testsimulation[["TFP"]][[201]], delta)
# 
# steadystate_checker(testsimulation[nrow(testsimulation), ],
#                                paragrid[nrow(paragrid), ],
#                                "ESEG")

# VisualiseSimulation(testsimulation, variable_encoder("Capital Stock per Worker"), "free")

