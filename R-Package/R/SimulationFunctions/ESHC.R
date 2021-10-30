### 4.0 Extended Solow Growth Model #############################

# Meta-Information All Variables =================================
meta_ESHC_variables <-
    c(
        "Total Factor Productivity",
        "Human Capital Stock",
        "Capital Stock",
        "Labor Stock",
        "Output",
        
        "Human Capital Stock per Worker",
        "Capital Stock per Worker",
        "Output per Worker",
        
        "Human Capital Stock per Effective Worker",
        "Capital Stock per Effective Worker",
        "Output per Effective Worker",
        
        "Wage Rate",
        "Rental Rate",
        
        "Log of Output",
        "Log of Output per Worker",
        "Log of Output per Effective Worker",
        
        "Log of Capital Stock",
        "Log of Capital Stock per Worker",
        "Log of Capital Stock per Effective Worker",
        
        "Log of Human Capital Stock",
        "Log of Human Capital Stock per Worker",
        "Log of Human Capital Stock per Effective Worker",
        
        "Growth Rate of Output",
        "Growth Rate of Output per Worker",
        "Growth Rate of Output per Effective Worker",
        
        "Growth Rate of Capital Stock",
        "Growth Rate of Capital Stock per Worker",
        "Growth Rate of Capital Stock per Effective Worker",
        
        "Growth Rate of Human Capital Stock",
        "Growth Rate of Human Capital Stock per Worker",
        "Growth Rate of Human Capital Stock per Effective Worker"
    )

SimulateExtendedSolowModelHumanCapital <- function(paragrid, np, startvals){

  # Roxygen Header ---------------------------------
  #' @title Simulates the ESHC Solow variant
  #' @description Simulates all (both primary and secondary) endogenous variables to the extended Solow growth model with human capital.
  #' @inheritParams SimulateBasicSolowModel
  #' @note The structural equations to this model can be found in the vignette to this package:
  #' \code{vignette("SolowVariants")}
  #' @export

  # Function ---------------------------------

    # Inputs ---------------------------------
    # paragrid for parameter grid;
    # np for number of periods;
    
    # Load Basic Model Functions ---------------------------------
    source("ModelFunctions/ESHCModelFunctions.R")
    
    # Initialize Simulation Table ---------------------------------
    sim_table <- create_simulation_table(variable_encoder(meta_ESHC_variables), np)
    # Fill Start Values for Period 0 ---------------------------------
    aux_index <- which(sim_table$period == 0)
    sim_table[[aux_index, "TFP"]] <- startvals$A
    sim_table[[aux_index, "H"]] <- startvals$H
    sim_table[[aux_index, "K"]] <- startvals$K
    sim_table[[aux_index, "L"]] <- startvals$L
    
    sim_table[[aux_index, "Y"]] <- ESHC_MF_Y(sim_table[[aux_index, "TFP"]],
                                             sim_table[[aux_index, "H"]],
                                             sim_table[[aux_index, "K"]],
                                             sim_table[[aux_index, "L"]], 
                                             paragrid[["alpha"]][[which(paragrid$period == 0)]],
                                             paragrid[["phi"]][[which(paragrid$period == 0)]]
                                             )

    # Computing Variables after Period 0 ---------------------------------
    for (i in 1:np){
        # i <- 1
        aux_index <- which(sim_table$period == i)
        
        sim_table[[aux_index, "TFP"]] <- ESHC_MF_AN(paragrid[[aux_index - 1, "g"]],
                                                sim_table[[aux_index - 1, "TFP"]])
        
        sim_table[[aux_index, "L"]] <- ESHC_MF_LN(paragrid[[aux_index - 1, "n"]],
                                                sim_table[[aux_index - 1, "L"]])
        
        sim_table[[aux_index, "K"]] <- ESHC_MF_KN(paragrid[[aux_index - 1, "sK"]],
                                                  sim_table[[aux_index - 1, "Y"]],
                                                  paragrid[[aux_index - 1, "delta"]],
                                                  sim_table[[aux_index -1 , "K"]])
        
        sim_table[[aux_index, "H"]] <- ESHC_MF_HN(paragrid[[aux_index - 1, "sH"]],
                                                  sim_table[[aux_index - 1, "Y"]],
                                                  paragrid[[aux_index - 1, "delta"]],
                                                  sim_table[[aux_index -1 , "H"]])
        
        sim_table[[aux_index, "Y"]] <- ESHC_MF_Y(sim_table[[aux_index, "TFP"]], 
                                               sim_table[[aux_index, "H"]],
                                               sim_table[[aux_index, "K"]],
                                               sim_table[[aux_index, "L"]],
                                               paragrid[[aux_index, "alpha"]],
                                               paragrid[[aux_index, "phi"]])
        
    }
    
    # Computing Additional Variables ---------------------------------
    
    remaining_vars_to_compute_bool <- names(sim_table) %in% c("period", "TFP", "H", "K", "L", "Y")
    
    sim_table <- add_var_computer(sim_table, remaining_vars_to_compute_bool, paragrid, "endo", "ESHC")
    # View(sim_table)
    return(sim_table)
}

# Testing
# testnamel <- c("alpha", "phi", "n", "g", "sK", "sH", "delta")
# testivl <- c(1/3, 1/3, 0.1, 0.1, 0.2, 0.05, 0.1)
# testpfcl <- c(NA,NA,NA, NA, NA, NA, NA)
# testnvl <- c(NA, NA, NA, NA, NA, NA, NA)
# np <- 200
# testgridalt <- create_parameter_grid(testnamel, testivl, testpfcl, testnvl, np)
# paragrid <- testgridalt
# startvals <- list(L = 1, H = 1, K = 1, A = 1)
# testsimulation <- SimulateExtendedSolowModelHumanCapital(testgridalt, np,startvals)
# simulation_correctness_checker(testsimulation[nrow(testsimulation), ],
#                                paragrid[nrow(paragrid), ],
#                                "ESHC")
# # View(testsimulation)
# VisualiseSimulation(testsimulation, variable_encoder(meta_ESHC_variables[6:10]), "fixed")
# simulation_correctness_checker(testsimulation[nrow(testsimulation), ],
#                                paragrid[nrow(paragrid), ],
#                                "ESHC")
