### 7.0 Extended Solow Growth Model with Scarce Resources — Land and Oil #############################

SimulateExtendedSolowModelScarceResourceOilAndLand <- function(paragrid, np, startvals){


  # Roxygen Header ---------------------------------
  #' @title Simulates the ESSROL Solow variant
  #' @description Simulates all (both primary and secondary) endogenous variables to the extended Solow growth model with the scarce resources of oil *and* land.
  #' @inheritParams SimulateBasicSolowModel
  #' @note The structural equations to this model can be found in the vignette to this package:
  #' \code{vignette("SolowVariants")}
  #' @export

  # Function ---------------------------------

    # Load Basic Model Functions ---------------------------------
    # source("ModelFunctions/ESSROLModelFunctions.R")
    
    # Initialize Simulation Table ---------------------------------
    sim_table <- create_simulation_table(variable_encoder(meta_ESSROL_variables), np)
    # Fill Start Values for Period 0 ---------------------------------
    aux_index <- which(sim_table$period == 0)
    sim_table[[aux_index, "TFP"]] <- startvals$A
    sim_table[[aux_index, "L"]] <- startvals$L
    sim_table[[aux_index, "K"]] <- startvals$K
    sim_table[[aux_index, "R"]] <- startvals$R
    sim_table[[aux_index, "E"]] <- ESSROL_MF_E(paragrid[[aux_index, "sE"]], sim_table[[aux_index, "R"]])
    sim_table[[aux_index, "Y"]] <- ESSROL_MF_Y(sim_table[[aux_index, "TFP"]],
                                              sim_table[[aux_index, "K"]],
                                              sim_table[[aux_index, "L"]],
                                              sim_table[[aux_index, "E"]],
                                              paragrid[[aux_index, "X"]],
                                              paragrid[[aux_index, "alpha"]],
                                              paragrid[[aux_index, "beta"]],
                                              paragrid[[aux_index, "kappa"]])
    
    # Computing Variables after Period 0 ---------------------------------
    for (i in 1:np){
        # i <- 1
        aux_index <- which(sim_table$period == i)
        sim_table[[aux_index, "TFP"]] <- ESSROL_MF_AN(paragrid[["g"]][[which(paragrid$period == i-1)]],
                                                sim_table[["TFP"]][[which(sim_table$period == i-1)]])
        sim_table[[aux_index, "L"]] <- ESSROL_MF_LN(paragrid[["n"]][[which(paragrid$period == i-1)]],
                                                sim_table[["L"]][[which(sim_table$period == i-1)]])
        sim_table[[aux_index, "K"]] <- ESSROL_MF_KN(paragrid[["s"]][[which(paragrid$period == i-1)]],
                                                sim_table[["Y"]][[which(sim_table$period == i-1)]],
                                                paragrid[["delta"]][[which(paragrid$period == i-1)]],
                                                sim_table[["K"]][[which(sim_table$period == i-1)]])
        
        sim_table[[aux_index, "R"]] <- ESSROL_MF_RN(sim_table[[aux_index -1, "E"]], sim_table[[aux_index - 1, "R"]])
        sim_table[[aux_index, "E"]] <- ESSROL_MF_E(paragrid[[aux_index, "sE"]], sim_table[[aux_index, "R"]])
        
        sim_table[[aux_index, "Y"]] <- ESSROL_MF_Y(sim_table[[aux_index, "TFP"]],
                                                   sim_table[[aux_index, "K"]],
                                                   sim_table[[aux_index, "L"]],
                                                   sim_table[[aux_index, "E"]],
                                                   paragrid[[aux_index, "X"]],
                                                   paragrid[[aux_index, "alpha"]],
                                                   paragrid[[aux_index, "beta"]],
                                                   paragrid[[aux_index, "kappa"]])
    }
    
    # Computing Additional Variables ---------------------------------
    
    remaining_vars_to_compute_bool <- names(sim_table) %in% c("period","TFP", "L", "K", "Y", "E", "R")
    
    sim_table <- add_var_computer(sim_table, remaining_vars_to_compute_bool, paragrid, "endo", "ESSRO")
    
    return(sim_table)
}