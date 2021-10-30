### 2.0 General Solow Growth Model #############################

# Meta-Information All Variables =================================
meta_GS_variables <-
    c(
        "Capital Stock",
        "Labor Stock",
        "Output",
        "Consumption",
        
        "Capital Stock per Worker",
        "Output per Worker",
        "Consumption per Worker",
        
        "Capital Stock per Effective Worker",
        "Output per Effective Worker",
        "Consumption per Effective Worker",
        
        "Wage Rate",
        "Rental Rate",
        
        "Log of Output",
        "Log of Output per Worker",
        "Log of Output per Effective Worker",
        
        "Log of Capital Stock",
        "Log of Capital Stock per Worker",
        "Log of Capital Stock per Effective Worker",
        
        "Growth Rate of Output",
        "Growth Rate of Output per Worker",
        "Growth Rate of Output per Effective Worker",
        
        "Growth Rate of Capital Stock",
        "Growth Rate of Capital Stock per Worker",
        "Growth Rate of Capital Stock per Effective Worker",
        
        "Total Factor Productivity"
    )

SimulateGeneralSolowModel <- function(paragrid, np, startvals){

  # Roxygen Header ---------------------------------
  #' @title Simulates the General Solow Growth Model
  #' @description Simulates all (both primary and secondary) endogenous variables to the general Solow growth model.
  #' @inheritParams SimulateBasicSolowModel
  #' @note The structural equations to this model can be found in the vignette to this package:
  #' \code{vignette("SolowVariants")}
  #' @export

  # Function ---------------------------------
      
    # Load Basic Model Functions ---------------------------------
    source("ModelFunctions/GSModelFunctions.R")
    
    # Initialize Simulation Table ---------------------------------
    sim_table <- create_simulation_table(variable_encoder(meta_GS_variables), np)
    # Fill Start Values for Period 0 ---------------------------------
    aux_index <- which(sim_table$period == 0)
    sim_table[[aux_index, "TFP"]] <- startvals$A
    sim_table[[aux_index, "L"]] <- startvals$L
    sim_table[[aux_index, "K"]] <- startvals$K
    sim_table[[aux_index, "Y"]] <- GS_MF_Y(sim_table[["TFP"]][[which(sim_table$period == 0)]], 
                                           sim_table[["K"]][[which(sim_table$period == 0)]],
                                           sim_table[["L"]][[which(sim_table$period == 0)]],
                                           paragrid[["alpha"]][[which(paragrid$period == 0)]])
    # Computing Variables after Period 0 ---------------------------------
    for (i in 1:np){
        # i <- 1
        aux_index <- which(sim_table$period == i)
        sim_table[[aux_index, "TFP"]] <- GS_MF_AN(paragrid[["g"]][[which(paragrid$period == i-1)]],
                                                sim_table[["TFP"]][[which(sim_table$period == i-1)]])
        sim_table[[aux_index, "L"]] <- GS_MF_LN(paragrid[["n"]][[which(paragrid$period == i-1)]],
                                                sim_table[["L"]][[which(sim_table$period == i-1)]])
        sim_table[[aux_index, "K"]] <- GS_MF_KN(paragrid[["s"]][[which(paragrid$period == i-1)]],
                                                sim_table[["Y"]][[which(sim_table$period == i-1)]],
                                                paragrid[["delta"]][[which(paragrid$period == i-1)]],
                                                sim_table[["K"]][[which(sim_table$period == i-1)]])
        
        sim_table[[aux_index, "Y"]] <- GS_MF_Y(sim_table[["TFP"]][[which(sim_table$period == i)]], 
                                               sim_table[["K"]][[which(sim_table$period == i)]],
                                               sim_table[["L"]][[which(sim_table$period == i)]],
                                               paragrid[["alpha"]][[which(paragrid$period == i)]])
    }
    
    # Computing Additional Variables ---------------------------------
    
    remaining_vars_to_compute_bool <- names(sim_table) %in% c("period","TFP", "L", "K", "Y")
    
    sim_table <- add_var_computer(sim_table, remaining_vars_to_compute_bool, paragrid, "endo", "GS")
    
    return(sim_table)
}

# # Testing
# testnamel <- c("g", "alpha", "delta", "n", "s")
# testivl <- c(0.1, 1/3,0.1, 0.04, 0.23)
# testpfcl <- c(NA,NA,NA, NA, NA)
# testnvl <- c(NA, NA, NA, NA, NA)
# np <- 200
# testgridalt <- create_parameter_grid(testnamel, testivl, testpfcl, testnvl, np)
# paragrid <- testgridalt
# startvals <- list(K = 1, L = 1, A = 1)
# testsimulation <- SimulateGeneralSolowModel(testgridalt, np,startvals)
# simulation_correctness_checker(testsimulation[nrow(testsimulation), ],
#                                paragrid[nrow(paragrid), ],
#                                "GS")
# # View(testsimulation)
# VisualiseSimulation(testsimulation, variable_encoder(meta_GS_variables[1:3]), "free")
