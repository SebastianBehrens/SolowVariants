### 11.0 Extended Solow Growth Model with Endogenous Growth (Cozzi's Hybrid Model)#############################

SimulateExtendedSolowModelEndogenousGrowthCozziTwo <- function(paragrid, np, startvals){

  # Roxygen Header ---------------------------------
  #' @title Simulates the ESEGCozziTwo Solow variant
  #' @description Simulates all (both primary and secondary) endogenous variables to the extended Solow growth model with endogenous technological growth as in the version put forward by Cozzi
  #' @note The structural equations to this model can be found in the vignette to this package:
  #' \code{vignette("SolowVariants")}
  #' @inheritParams SimulateBasicSolowModel
  #' @export

  # Function ---------------------------------
    
    # Remark on n = 0 in case of fully endogenous growth (phi -> 1) ---------------------------------
    if(any(unique(paragrid[["phi"]]) %>% between (0.95, 1))){
        if(any(unique(paragrid[["n"]])!=0)){
        message("Given the entered value for phi is close to 1, which approximates the fully endogenous ESEG, the parameter n should be set at 0.")
    }

    }
    
    # Load Basic Model Functions ---------------------------------
    # source("ModelFunctions/ESEGCozziTwoModelFunctions.R")
    
    # Initialize Simulation Table ---------------------------------
    sim_table <- create_simulation_table(variable_encoder(getModelVars("ESEGCozziTwo")), np)
    
    # Fill Start Values for Period 0 ---------------------------------
    aux_index <- which(sim_table$period == 0)
    sim_table[[aux_index, "L"]] <- startvals$L
    sim_table[[aux_index, "K"]] <- startvals$K
    sim_table[[aux_index, "TFP"]] <- startvals$A
    sim_table[[aux_index, "Y"]] <- ESEGCozziTwo_MF_Y(
        sim_table[["TFP"]][[which(sim_table$period == 0)]],
        sim_table[["K"]][[which(sim_table$period == 0)]],
        sim_table[["L"]][[which(sim_table$period == 0)]]* (1-paragrid[["sR"]][[which(paragrid$period == 0)]]),
        paragrid[["alpha"]][[which(paragrid$period == 0)]]
    )
    
    # Computing Variables after Period 0 ---------------------------------
    for (i in 1:np){
        # i <- 1
        # print(i)

        aux_index <- which(sim_table$period == i)

        sim_table[[aux_index, "TFP"]] <- ESEGCozziTwo_MF_AN(
          paragrid[["k"]][[which(paragrid$period == i-1)]],
          paragrid[["rho"]][[which(paragrid$period == i-1)]],
          paragrid[["phi"]][[which(paragrid$period == i-1)]],
          paragrid[["lambda"]][[which(paragrid$period == i-1)]],
          sim_table[["TFP"]][[which(sim_table$period == i-1)]],
          paragrid[["sR"]][[which(paragrid$period == i-1)]],
          sim_table[["L"]][[which(sim_table$period == i-1)]] * paragrid[["sR"]][[which(paragrid$period == i-1)]]
          )

        sim_table[[aux_index, "L"]] <- ESEGCozziTwo_MF_LN(
          paragrid[["n"]][[which(paragrid$period == i-1)]],
          sim_table[["L"]][[which(sim_table$period == i-1)]])
        
        sim_table[[aux_index, "K"]] <- ESEGCozziTwo_MF_KN(
          paragrid[["s"]][[which(paragrid$period == i-1)]],
          sim_table[["Y"]][[which(sim_table$period == i-1)]],
          paragrid[["delta"]][[which(paragrid$period == i-1)]],
          sim_table[["K"]][[which(sim_table$period == i-1)]])
        
        sim_table[[aux_index, "Y"]] <- ESEGCozziTwo_MF_Y(
            sim_table[["TFP"]][[aux_index]],
            sim_table[["K"]][[aux_index]],
            sim_table[["L"]][[aux_index]]* (1-paragrid[["sR"]][[aux_index]]),
            paragrid[["alpha"]][[aux_index]]
            )
    }
    
    # Computing Additional Variables ---------------------------------
    
    remaining_vars_to_compute_bool <- names(sim_table) %in% c("period", "TFP", "L", "K", "Y")
    
    sim_table <- add_var_computer(sim_table, remaining_vars_to_compute_bool, paragrid, "endo", "ESEGCozziTwo")
    
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

