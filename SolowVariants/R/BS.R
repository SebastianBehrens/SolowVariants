### 1.0 Basic Solow Growth Model #############################

SimulateBasicSolowModel <- function(paragrid, np, startvals){

  # Roxygen Header ---------------------------------
  #' @title Simulates the Basic Solow Growth Model
  #' @description Simulates all (both primary and secondary) endogenous variables to the basic Solow growth model.
  #' @param paragrid Parameter grid from \code{create_parameter_grid()}.
  #' @param np Number of periods.
  #' @param startvals List with starting values for the variables A, K, L, H, #' R, V. **Remark**: Use \code{getRequiredStartingValues(ModelCode)} to see which variables need to have a starting value in the respective Solow model.
  #' @note The structural equations to this model can be found in the vignette to this package:
  #' \code{vignette("SolowVariants")}
  #' @export

  # Function ---------------------------------
    # Inputs ---------------------------------

    # Load Basic Model Functions ---------------------------------
    # source("ModelFunctions/BSModelFunctions.R")

    # Initialize Simulation Table ---------------------------------
    sim_table <- create_simulation_table(variable_encoder(meta_BS_variables), np)

    # Fill Start Values for Period 0 ---------------------------------
    aux_index <- which(sim_table$period == 0)
    sim_table[[aux_index, "L"]] <- startvals$L
    sim_table[[aux_index, "K"]] <- startvals$K
    sim_table[[aux_index, "Y"]] <- BS_MF_Y(paragrid[["B"]][[which(paragrid$period == 0)]],
                                           sim_table[["K"]][[which(sim_table$period == 0)]],
                                           sim_table[["L"]][[which(sim_table$period == 0)]],
                                           paragrid[["alpha"]][[which(paragrid$period == 0)]])

    # Computing Variables after Period 0 ---------------------------------
    for (i in 1:np){
        # i <- 1
        # print(i)
        aux_index <- which(sim_table$period == i)
        sim_table[[aux_index, "L"]] <- BS_MF_LN(paragrid[["n"]][[which(paragrid$period == i-1)]],
                                                sim_table[["L"]][[which(sim_table$period == i-1)]])
        sim_table[[aux_index, "K"]] <- BS_MF_KN(paragrid[["s"]][[which(paragrid$period == i-1)]],
                                                sim_table[["Y"]][[which(sim_table$period == i-1)]],
                                                paragrid[["delta"]][[which(paragrid$period == i-1)]],
                                                sim_table[["K"]][[which(sim_table$period == i-1)]])
        sim_table[[aux_index, "Y"]] <- BS_MF_Y(paragrid[["B"]][[which(paragrid$period == i)]],
                                               sim_table[["K"]][[which(sim_table$period == i)]],
                                               sim_table[["L"]][[which(sim_table$period == i)]],
                                               paragrid[["alpha"]][[which(paragrid$period == i)]])
    }

    # Computing Additional Variables ---------------------------------

    remaining_vars_to_compute_bool <- names(sim_table) %in% c("period", "L", "K", "Y")

    sim_table <- add_var_computer(sim_table, remaining_vars_to_compute_bool, paragrid, "exo", "BS")

    return(sim_table)
}

# # Testing
# testnamel <- c("B", "alpha", "delta", "n", "s")
# testivl <- c(10, 1/3,0.1, 0.005, 0.22)
# testpfcl <- c(NA,NA,NA, NA, NA)
# testnvl <- c(NA, NA, NA, NA, NA)
# testgridalt <- create_parameter_grid(testnamel, testivl, testpfcl, testnvl, np)
# paragrid <- testgridalt
# np <- 200
# startvals <- list(K = 1, L = 1)
# testsimulation <- SimulateBasicSolowModel(testgridalt, np,startvals)
#
#
# simulation_correctness_checker(testsimulation[nrow(testsimulation), ],
#                                paragrid[nrow(paragrid), ],
#                                "BS")

# # # # View(testsimulation)
# VisualiseSimulation(testsimulation, variable_encoder("Capital Stock per Worker"), "free")
