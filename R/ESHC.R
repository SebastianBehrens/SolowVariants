### 4.0 Extended Solow Growth Model #############################

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
    # source("ModelFunctions/ESHCModelFunctions.R")
    
    # Initialize Simulation Table ---------------------------------
    sim_table <- create_simulation_table(variable_encoder(getModelVars("ESHC")), np)
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