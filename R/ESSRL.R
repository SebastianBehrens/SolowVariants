### 6.0 Extended Solow Growth Model with Scarce Resources — Land #############################

SimulateExtendedSolowModelScarceResourceLand <- function(paragrid, np, startvals){

  # Roxygen Header ---------------------------------
  #' @title Simulates the ESSRL Solow variant
  #' @description Simulates all (both primary and secondary) endogenous variables to the extended Solow growth model with the scarce resource of land.
  #' @inheritParams SimulateBasicSolowModel
  #' @note The structural equations to this model can be found in the vignette to this package:
  #' \code{vignette("SolowVariants")}
  #' @export

  # Function ---------------------------------

    # Load Basic Model Functions ---------------------------------
    # source("ModelFunctions/ESSRLModelFunctions.R")
    # source("HelperFunctions.R")
    
    # Initialize Simulation Table ---------------------------------
    sim_table <- create_simulation_table(variable_encoder(getModelVars("ESSRL")), np)
    # Fill Start Values for Period 0 ---------------------------------
    aux_index <- which(sim_table$period == 0)
    sim_table[[aux_index, "TFP"]] <- startvals$A
    sim_table[[aux_index, "L"]] <- startvals$L
    sim_table[[aux_index, "K"]] <- startvals$K 
    
    sim_table[[aux_index, "Y"]] <- ESSRL_MF_Y(sim_table[[aux_index, "TFP"]], 
                                              sim_table[[aux_index, "K"]],
                                              sim_table[[aux_index, "L"]],
                                              paragrid[[aux_index, "X"]],
                                              paragrid[[aux_index, "alpha"]],
                                              paragrid[[aux_index, "beta"]])
    # Computing Variables after Period 0 ---------------------------------
    for (i in 1:np){
        # i <- 1
        aux_index <- which(sim_table$period == i)
        
        sim_table[[aux_index, "TFP"]] <- ESSRL_MF_AN(paragrid[[aux_index - 1, "g"]],
                                                   sim_table[[aux_index - 1, "TFP"]])
        sim_table[[aux_index, "K"]] <- ESSRL_MF_KN(paragrid[[aux_index -1, "s"]],
                                                   sim_table[[aux_index - 1, "Y"]],
                                                   paragrid[[aux_index -1, "delta"]], 
                                                   sim_table[[aux_index -1, "K"]])
        
        sim_table[[aux_index, "L"]] <- ESSRL_MF_LN(paragrid[[aux_index - 1, "n"]],
                                                  sim_table[[aux_index - 1, "L"]])
        
        sim_table[[aux_index, "Y"]] <- ESSRL_MF_Y(sim_table[[aux_index, "TFP"]],
                                                  sim_table[[aux_index, "K"]],
                                                  sim_table[[aux_index, "L"]],
                                                  paragrid[[aux_index, "X"]],
                                                  paragrid[[aux_index, "alpha"]],
                                                  paragrid[[aux_index, "beta"]])
    }
    
    # Computing Additional Variables ---------------------------------
    
    remaining_vars_to_compute_bool <- names(sim_table) %in% c("period", "TFP", "K", "L", "Y")
    
    sim_table <- add_var_computer(sim_table, remaining_vars_to_compute_bool, paragrid, "endo", "ESSRL")
    # View(sim_table)
    return(sim_table)
}