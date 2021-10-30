# This file sets up several helper functions that will be used throughout the Solow model simulation functions.

# 0.1 individual parameter path =================================
create_path <- function(iv, pfc, nv, np){

  # Roxygen Header ---------------------------------
  #' @title Creates the a single parameter vector.
  #' @description Creates vector with \code{np+1} entries of \code{iv}
  #' (or optionally \code{nv} from period \code{pfc} onwards).
  #' @details the "parameter path" refers to the series of
  #' parameter values along all np+1 periods
  #' @param iv initial value of parameter;
  #' @param pfc period of change
  #' (\code{pfc = NA} is a special entry! see examples.)
  #' @param nv new value of parameter (starting in period of change)
  #' (\code{nv = NA} is a special entry! see examples.)
  #' @param np total number of periods
  #' additional line to parameter np
  #' @examples
  #' create_path(1, NA, NA, 5)
  #' create_path(3, 5, 4, 10)
  #' @export

  # Function ---------------------------------

  # first: catching nonsensical inputs
  if(!is.na(pfc)){
  # if the period of change occurs in later periods than a simulation is
  #                                 created for, then it does not matter.
    if(pfc > np){
      pfc <- NA
    }
    # catching negative periods of change
    if(pfc < 0){
      pfc <- abs(pfc)
    }

  }

  # second: filling in vector (so-called parameter path)
  if(!is.na(pfc)){
    part1 <- rep(iv, pfc)
    part2 <- rep(nv, np, np-pfc+1)
  }else{
    part1 <- c()
    part2 <- rep(iv, np+1)
  }
  return(c(part1, part2))
}
# testing
# create_path(2, 3, 4, 7)

# 0.2 grid of individual parameter paths =================================
create_parameter_grid <- function(namel, ivs, pfcs, nvs, np){

  # Roxygen Header ---------------------------------
  #' @title Create a set of parameter pathes (grid).
  #' @description Creates dataframe with \code{np+1} rows and
  #' \code{length(namel)} columns.
  #' @details The word "grid" refers to multiple parameter pathes.
  #' The term will be referenced throughout the simulation functions
  #' as every model is defined via exogenously given parameters that will
  #' be supplied and referenced as a 'parameter-grid' in the simulation functions.
  #' **Warning**: Make sure you use the correct names in \code{names} for the respective parameters. Use \code{getRequiredParams(ModelCode)} to get them. The model abbreviations for \code{ModelCode} (used consistently throughout the package) can be found in the vignette.
  #' **Warning**: The inputs will be used to create the paramter pathes. For each paramter, the same index will be used across all input vectors. That means that the first entry to \code{ivs} will be the initial value to the parameter that is the first entry in the \code{namel} vector. Same applies the rest of the inputs to this function.
  #' @param names A vector with the names of the model-specific set of exogenously defined parameters. Refer to the vignette for the used names to the parameters.
  #' @param ivs Vector with intial values of the parameters.
  #' @param pfcs Vector with periods of paramter changes.
  #' @param nvs Vector with new paramter values from the respective period of paramter change onwards.
  #' @param np Integer indicating the number of periods for which the paramter-grid will be set up.
  #' @examples
  #' create_parameter_grid(getRequiredParams("BS"),
  #'                        c(1, 1/3, 0.1, 0.02, 0.15),
  #'                        c(NA, NA, NA, NA, NA),
  #'                        c(NA, NA, NA, NA, NA),
  #'                        10)
  #'
  #' @export

  # Function ---------------------------------
  aux <- tibble(period = c(0:np))
  for(i in seq_along(namel)){
    aux_path <- create_path(ivs[[i]], pfcs[[i]], nvs[[i]], np)
    aux[[namel[[i]] ]] <- aux_path
  }
  return(aux)
}

# testing of create_paramter_grid
# testnamel <- c("B", "alpha", "delta", "n", "s")
# testivl <- c(1,1/3,0.1, 0.04, 0.23)
# testpfcl <- c(NA,NA, NA, NA, NA)
# testnvl <- c(NA, NA, NA, NA, NA)
# testgrid <- create_parameter_grid(testnamel, testivl, testpfcl, testnvl, 50)


# 0.3 grid of paths of model variables =================================
create_simulation_table <- function(vts, np) {

  # Roxygen Header ---------------------------------
  #' @title Creates for the simulated endo. variables
  #' @description Creates an empty tibble with \code{np}+1 rows and a column for each entry in \code{vts}. See the documentation for the argument \code{vts} below.
  #' @details This function will be used in the simulation functions to set up the grid that will then be in the said simulation function.
  #' @param vts Vector with the abbreviation for variables that should be simulated. (vts for 'variables to simulate') The exhaustive list with all variables that can be simulated is given in the beginning of the respective simulation function.
  #' @examples
  #' create_simulation_table(variable_encoder(meta_BS_variables), 10)
  #' @export

  # Function ---------------------------------

  aux <- tibble(period = c(0:np))
  for (i in c(1:length(vts))) {
    aux_sequence <- rep(NA, np + 1)
    aux[[vts[[i]]]] <- as.double(aux_sequence)
  }
  return(aux)
}


# 0.5 encoding variables =================================
variable_encoder <- function(variables){

  # Roxygen Header ---------------------------------
  #' @title Switch between variable abbreviations and full variables names
  #' @description Encode variables, that is, to encode the full variable variable name as defined at the top of simulation files and encode them into their respective abbreviation.
  #' @param variables Vector of length greater or equal to one containing the full variable name(s).
  #' @examples
  #' variable_encoder("Total Factor Productivity")
  #' variable_encoder(meta_BS_variables)
  #' @export

  # Function ---------------------------------

  n_vars <- length(variables)
  aux <- as.double(rep(NA, n_vars))
  for(i in 1:n_vars){
    aux2 <- variables[[i]]
    aux3 <- case_when(
      aux2 == "Total Factor Productivity" ~ "TFP",

      aux2 == "Human Capital Stock" ~ "H",
      aux2 == "Log of Human Capital Stock" ~ "logH",
      aux2 == "Capital Stock" ~ "K",
      aux2 == "Log of Capital Stock" ~ "logK",
      aux2 == "Growth Rate of Capital Stock" ~ "gK",
      aux2 == "Growth Rate of Human Capital Stock" ~ "gH",

      aux2 == "Capital Stock per Worker" ~ "KpW",
      aux2 == "Log of Capital Stock per Worker" ~ "logKpW",
      aux2 == "Growth Rate of Capital Stock per Worker" ~ "gKpW",

      aux2 == "Human Capital Stock per Worker" ~ "HpW",
      aux2 == "Log of Human Capital Stock per Worker" ~ "logHpW",
      aux2 == "Growth Rate of Human Capital Stock per Worker" ~ "gHpW",

      aux2 == "Capital Stock per Effective Worker" ~ "KpEW",
      aux2 == "Log of Capital Stock per Effective Worker" ~ "logKpEW",
      aux2 == "Growth Rate of Capital Stock per Effective Worker" ~ "gKpEW",

      aux2 == "Human Capital Stock per Effective Worker" ~ "HpEW",
      aux2 == "Log of Human Capital Stock per Effective Worker" ~ "logHpEW",
      aux2 == "Growth Rate of Human Capital Stock per Effective Worker" ~ "gHpEW",

      aux2 == "Labor Stock" ~ "L",

      aux2 == "Wage Rate" ~ "WR",
      aux2 == "Rental Rate" ~ "RR",
      aux2 == "Capital Rental Rate" ~ "RR",
      aux2 == "Land Rental Rate" ~ "LR",

      aux2 == "Output" ~ "Y",
      aux2 == "Log of Output" ~ "logY",
      aux2 == "Growth Rate of Output" ~ "gY",

      aux2 == "Output per Worker" ~ "YpW",
      aux2 == "Log of Output per Worker" ~ "logYpW",
      aux2 == "Growth Rate of Output per Worker" ~ "gYpW",

      aux2 == "Output per Effective Worker" ~ "YpEW",
      aux2 == "Log of Output per Effective Worker" ~ "logYpEW",
      aux2 == "Growth Rate of Output per Effective Worker" ~ "gYpEW",

      aux2 == "National Output" ~ "Yn",
      aux2 == "National Wealth" ~ "V",
      aux2 == "National Wealth per Worker"~ "VpW",
      aux2 == "Net Foreign Assets" ~ "F",
      aux2 == "Net Foreign Assets per Worker" ~ "FpW",
      aux2 == "National Savings" ~ "Sn",

      aux2 == "Energy Use" ~ "E",
      aux2 == "Resource Stock" ~ "R",

      aux2 == "Consumption" ~ "C",
      aux2 == "Consumption per Worker" ~ "CpW",
      aux2 == "Consumption per Effective Worker" ~ "CpEW",

      aux2 == "Capital to Output Ratio" ~ "CtO"



    )
    aux[[i]] <- aux3

  }
  return(aux)
}


# 0.6 visualise a simulation =================================
VisualiseSimulation <- function(simulation_data, variables, scale_identifier){

  # Roxygen Header ---------------------------------
  #' @title Visualise selected variables of a simulation
  #' @description
  #' @details
  #' @param simulation_data The output tibble of any simulation function.
  #' @param variables The variables to visualise in *abbreviated* form.
  #' @param scale_identifier string to indicate freely floating scales ("free") or fixed sales ("fixed")
  #' @export

  # Function ---------------------------------
  variables <- c("period", variables)
  simulation_data %>% select(all_of(variables)) %>%
    pivot_longer(-period, names_to = "Variable") %>%
    mutate(Variable = as.factor(Variable)) %>%
    ggplot(aes(period, value, col = Variable)) +
    geom_line() +
    facet_wrap(~Variable, scales = scale_identifier, ncol = 2)+
    labs(x = "Period", y = "Value") +
    theme(legend.position = "none")
}


# 0.7 computing additional variables =================================
add_var_computer <- function(sim_data, add_vars, parameter_data, technology_variant, solowversion){

  # Roxygen Header ---------------------------------
  #' @title Compute the non-primary variables
  #' @description This function computes additional secondary variables that are not computed in a models respective simulation functions. The simulation functions only compute the primary variables of a model such as L, K and Y in the Basic Solow Growth Model (BS).
  #' @param sim_data The tibble that is being filled in the simulation function.
  #' @param add_vars Vector with the encoded variables that are secondary and need to be computed to make the simulation table complete.
  #' @param parameter_data The output from \code{create_parameter_grid(...)}.
  #' @param technology_variant A string indicating the exogenous (\code{technology_variant = "exo"}) or endogenous (\code{technology_variant = "endo"}) nature of technology in the respective model. A special case (\code{technology_variant = "special"}) is the technology form of endogenous technology growth (model code "ESEG").
  #' @param solowversion The model code for the model, such as "BS" or "ESSOE". (The same variables such as WR or RR are computed differently depending on the model.)
  #' @note \text{See} SimulateBasicSolowModel() \text{for an example.}
  #' @export

  # Function ---------------------------------

  ## Testing (these variables will need to be computed in memory as a result of a step-by-step execution of the code in a simulation function until the point where this function is called.)
  # sim_data <- sim_table
  # add_vars <- remaining_vars_to_compute_bool
  # parameter_data <- paragrid
  # technology_variant <- "exo"
  # solowversion <- "ESSOE"


  # first: filling in the variable technology depending on the \code{technology_variant} to make the same code useable for different forms of technology:
  # A (endogeneous variable) and B (parameter) in the different Solow Models
  if(technology_variant == "endo"){
    technology <- sim_data[["TFP"]]
  }else if(technology_variant == "exo"){
    technology <- parameter_data[["B"]]
  }else if(technology_variant == "special"){
    technology <- sim_data[["K"]]^(parameter_data[["phi"]])
  }else{
    stop("Technology location unclear")
  }
  # second: computing and filling in the secondary endogenous variables
  for(i in names(sim_data)[!add_vars]){
    # Variants of Output
    if(i == "YpW"){sim_data[["YpW"]] <- sim_data[["Y"]]/sim_data[["L"]]}
    if(i == "YpEW"){sim_data["YpEW"] <- sim_data[["Y"]]/(technology * sim_data[["L"]])}
    # Variants of Output Logarithmised
    if(i == "logY"){sim_data[["logY"]] <- sim_data[["Y"]] %>% log()}
    if(i == "logYpW"){sim_data[["logYpW"]] <- sim_data[["YpW"]] %>% log()}
    if(i == "logYpEW"){sim_data[["logYpEW"]] <- sim_data[["YpEW"]] %>% log()}
    # Variants of Capital
    if(i == "KpW"){sim_data[["KpW"]] <- sim_data[["K"]]/sim_data[["L"]]}
    if(i == "KpEW"){sim_data["KpEW"] <- sim_data[["K"]]/(technology * sim_data[["L"]])}
    # Variants of Human Capital
    if(i == "HpW"){sim_data[["HpW"]] <- sim_data[["H"]]/sim_data[["L"]]}
    if(i == "HpEW"){sim_data["HpEW"] <- sim_data[["H"]]/(technology * sim_data[["L"]])}
    # Variants of Capital Logarithmised
    if(i == "logK"){sim_data[["logK"]] <- sim_data[["K"]] %>% log()}
    if(i == "logKpW"){sim_data[["logKpW"]] <- sim_data[["KpW"]] %>% log()}
    if(i == "logKpEW"){sim_data[["logKpEW"]] <- sim_data[["KpEW"]] %>% log()}
    # Variants of Human Capital Logarithmised
    if(i == "logH"){sim_data[["logH"]] <- sim_data[["H"]] %>% log()}
    if(i == "logHpW"){sim_data[["logHpW"]] <- sim_data[["HpW"]] %>% log()}
    if(i == "logHpEW"){sim_data[["logHpEW"]] <- sim_data[["HpEW"]] %>% log()}
    # Variants of Growth
    if(i == "gY"){sim_data[["gY"]] <- log(sim_data[["Y"]]) - log(lag(sim_data[["Y"]]))}
    if(i == "gYpW"){sim_data[["gYpW"]] <- log(sim_data[["YpW"]]) - log(lag(sim_data[["YpW"]]))}
    if(i == "gYpEW"){sim_data[["gYpEW"]] <- log(sim_data[["YpEW"]]) - log(lag(sim_data[["YpEW"]]))}
    if(i == "gK"){sim_data[["gK"]] <- log(sim_data[["K"]]) - log(lag(sim_data[["K"]]))}
    if(i == "gKpW"){sim_data[["gKpW"]] <- log(sim_data[["KpW"]]) - log(lag(sim_data[["KpW"]]))}
    if(i == "gKpEW"){sim_data[["gKpEW"]] <- log(sim_data[["KpEW"]]) - log(lag(sim_data[["KpEW"]]))}
    if(i == "gH"){sim_data[["gH"]] <- log(sim_data[["H"]]) - log(lag(sim_data[["H"]]))}
    if(i == "gHpW"){sim_data[["gHpW"]] <- log(sim_data[["HpW"]]) - log(lag(sim_data[["HpW"]]))}
    if(i == "gHpEW"){sim_data[["gHpEW"]] <- log(sim_data[["HpEW"]]) - log(lag(sim_data[["HpEW"]]))}
    # Variants of Saving
    if(i == "Sn"){sim_data[["Sn"]] <- parameter_data[["s"]] * sim_data[["Yn"]]}
    # Variants of Wealth and Foreign Assets in SOE Version
    if(i == "VpW"){sim_data[["VpW"]] <- sim_data[["V"]] / sim_data[["L"]]}
    if(i == "FpW"){sim_data[["FpW"]] <- sim_data[["F"]] / sim_data[["L"]]}
    # Variants of Consumption

    if(i == "C"){sim_data[["C"]] <- sim_data[["Y"]] * (1- parameter_data[["s"]])}
    if(i == "CpW"){sim_data[["CpW"]] <- sim_data[["C"]] / sim_data[["L"]]}
    if(i == "CpEW"){sim_data[["CpEW"]] <- sim_data[["C"]] / (technology * sim_data[["L"]])}

    if(i == "CtO"){sim_data[["CtO"]] <- sim_data[["KpW"]]/sim_data[["YpW"]]}


    # Variables uniquely calculated to different Solow Model Versions (e.g. WR, RR)
    # WR, RR for BS ---------------------------------
    if(solowversion == "BS") {
      source("ModelFunctions/BSModelFunctions.R")
      if (i == "WR") {
        sim_data[["WR"]] <- BS_MF_WR(technology,
                                     sim_data[["K"]],
                                     sim_data[["L"]],
                                     parameter_data[["alpha"]])
      }
      # Rental Rate
      if (i == "RR") {
        sim_data[["RR"]] <- BS_MF_RR(technology,
                                     sim_data[["K"]],
                                     sim_data[["L"]],
                                     parameter_data[["alpha"]])
      }

    }
    # WR, RR for GS ---------------------------------
    if(solowversion == "GS") {
      source("ModelFunctions/GSModelFunctions.R")
      if (i == "WR") {
        sim_data[["WR"]] <- GS_MF_WR(technology,
                                     sim_data[["K"]],
                                     sim_data[["L"]],
                                     parameter_data[["alpha"]])
      }
      # Rental Rate
      if (i == "RR") {
        sim_data[["RR"]] <- GS_MF_RR(technology,
                                     sim_data[["K"]],
                                     sim_data[["L"]],
                                     parameter_data[["alpha"]])
      }

    }
    # WR, RR for ESSOE ---------------------------------
    if(solowversion == "ESSOE") {
        source("ModelFunctions/ESSOEModelFunctions.R")
      if (i == "WR") {
        sim_data[["WR"]] <- ESSOE_MF_WR(technology,
                                     sim_data[["K"]],
                                     sim_data[["L"]],
                                     parameter_data[["alpha"]])

      }
      # Rental Rate
      if (i == "RR") {
        sim_data[["RR"]] <- ESSOE_MF_RR(technology,
                                     sim_data[["K"]],
                                     sim_data[["L"]],
                                     parameter_data[["alpha"]])
      }
    }
      # WR, RR for ESHC ---------------------------------
      if(solowversion == "ESHC") {
          source("ModelFunctions/ESHCModelFunctions.R")
        if (i == "WR") {
          sim_data[["WR"]] <- ESHC_MF_WR(technology,
                                         sim_data[["H"]],
                                         sim_data[["K"]],
                                         sim_data[["L"]],
                                         parameter_data[["alpha"]],
                                         parameter_data[["phi"]])

        }
        # Rental Rate
        if (i == "RR") {
          sim_data[["RR"]] <- ESHC_MF_RR(technology,
                                         sim_data[["H"]],
                                         sim_data[["K"]],
                                         sim_data[["L"]],
                                         parameter_data[["alpha"]],
                                         parameter_data[["phi"]])
        }






      }
    # WR, RR for ESSRL ---------------------------------
    if(solowversion == "ESSRL") {
        source("ModelFunctions/ESSRLModelFunctions.R")
      if (i == "WR") {
        sim_data[["WR"]] <- ESSRL_MF_WR(technology,
                                        sim_data[["K"]],
                                        sim_data[["L"]],
                                        parameter_data[["X"]],
                                        parameter_data[["alpha"]],
                                        parameter_data[["beta"]]
                                        )
      }
      # Rental Rate
      if (i == "RR") {
        sim_data[["RR"]] <- ESSRL_MF_RR(technology,
                                        sim_data[["K"]],
                                        sim_data[["L"]],
                                        parameter_data[["X"]],
                                        parameter_data[["alpha"]],
                                        parameter_data[["beta"]])
      }
      # Land Rental Rate
      if (i == "LR") {
        sim_data[["LR"]] <- ESSRL_MF_LR(technology,
                                        sim_data[["K"]],
                                        sim_data[["L"]],
                                        parameter_data[["X"]],
                                        parameter_data[["alpha"]],
                                        parameter_data[["beta"]])
      }



    }
    # WR, RR for ESSRO ---------------------------------
    if(solowversion == "ESSRO") {
        source("ModelFunctions/ESSROModelFunctions.R")
      if (i == "WR") {
        # The _WR and _RR functions don't exist yet, I will need to compute them by hand first. They are not given in the book.
        # sim_data[["WR"]] <- ESSRO_MF_WR()

      }
      # Rental Rate
      if (i == "RR") {
        # sim_data[["RR"]] <- ESSRO_MF_RR()
      }



    }

    # WR, RR and TFP for ESEG ---------------------------------
    if(solowversion == "ESEG") {
        source("ModelFunctions/ESEGModelFunctions.R")
      if(i == "TFP"){
        sim_data[["TFP"]] <- sim_data[["K"]]^parameter_data[["phi"]]
      }
      if (i == "WR") {
        # sim_data[["WR"]] <- ESEG_MF_WR(technology,
        #                                sim_data[["H"]],
        #                                sim_data[["K"]],
        #                                sim_data[["L"]],
        #                                parameter_data[["alpha"]],
        #                                parameter_data[["phi"]])
        #
      }
      # Rental Rate
      if (i == "RR") {
        # sim_data[["RR"]] <- ESEG_MF_RR(technology,
        #                                sim_data[["H"]],
        #                                sim_data[["K"]],
        #                                sim_data[["L"]],
        #                                parameter_data[["alpha"]],
        #                                parameter_data[["phi"]])
      }




    }



  }

  return(sim_data)
}

# 0.8 compute steady state values and check correctness of simulations =================================
simulation_correctness_checker <- function(last_row_simulation, last_row_parameter, solow_variant){

  # Roxygen Header ---------------------------------
  #' @title Check correctness by comparing simulated (endo.) variables to their steady state values
  #' @description Compare variables in the final period of the simulation to their respective steady state value (given the exo. paramters).
  #' @details This function presumes that the number of periods simulated were such that the final values are near steady state. If the steady state path is disrupted by a parameter change in the second to last period, this function will yield misleading results.
  #' @param last_row_simulation Vector of the values of both primary and secondary variables in the last period of the simulation.
  #' @param last_row_parameter Vector of the values of the exo. paramter (from the paramter grid).
  #' @param solow_variant String indicating the model, such as "BS" or "ESHC".
  #' @export

  # Function ---------------------------------

  # last_row for the last row of the simulation table (sim_table %>% tail(1))
  # solow_variant for the different solow variants
    aux <- tibble(variable = toString(NA), last_value = as.double(NA), steadystate = as.double(NA))
    aux[[1,1]] <- NA

    all_possible_steady_state_function_inputs <-
      list(
        delta = last_row_parameter[["delta"]],
        s = last_row_parameter[["s"]],
        sK = last_row_parameter[["sK"]],
        sH = last_row_parameter[["sH"]],
        sE = last_row_parameter[["sE"]],
        n = last_row_parameter[["n"]],
        B = last_row_parameter[["B"]],
        r = last_row_parameter[["r"]],
        g = last_row_parameter[["g"]],
        alpha = last_row_parameter[["alpha"]],
        beta = last_row_parameter[["beta"]],
        kappa = last_row_parameter[["kappa"]],
        phi = last_row_parameter[["phi"]],
        YpW = last_row_simulation[["YpW"]],
        RR = last_row_simulation[["RR"]],
        w = last_row_simulation[["WR"]],
        A = last_row_simulation[["TFP"]],
        KpW = last_row_simulation[["KpW"]],
        L = last_row_simulation[["L"]],
        X = last_row_parameter[["X"]],
        R = last_row_simulation[["R"]]
      )

    if(solow_variant == "BS") {
      # Remark: The selections variable_encoder(meta_GS_variables[c(6, 7, 8, 9)]) can be generally adjusted to simply c("KpW", "YpW", ...) as done for some
      aux_steadystate_variables <- c("KpW", "YpW", "CpW", "WR", "RR")
    }else if(solow_variant == "GS"){
      aux_steadystate_variables <- c("KpW", "YpW", "CpW", "WR", "RR", "KpEW", "YpEW")
    }else if(solow_variant == "ESSOE"){
      aux_steadystate_variables <- c("KpW", "YpW", "WR", "VpW", "FpW")
    }else if(solow_variant == "ESHC"){
      aux_steadystate_variables <- c("KpEW", "HpEW", "YpEW", "YpW", "CpW") # WR and RR missing
    }else if(solow_variant == "ESSRL"){
      aux_steadystate_variables <- c("CtO", "YpW") # WR and RR missing
    }else if(solow_variant == "ESSRO"){
      aux_steadystate_variables <- c("YpW")
    }else if(solow_variant == "ESSROL"){
      aux_steadystate_variables <- c("gY")
    }else if(solow_variant == "ESEG"){
      if(last_row_parameter[["phi"]] < 0.95){
      aux_steadystate_variables <- c("KpEW", "gYpW")
      }else if(last_row_parameter[["phi"]] %>% between(0.95, 1)){
      aux_steadystate_variables <- c("gYpW")
      }
    }

    for(i in aux_steadystate_variables){
      aux_function_name <- paste(solow_variant, "_SS_", i, sep = "")
      SS_val_computed <- doCall(aux_function_name, args = all_possible_steady_state_function_inputs)
      aux <- aux %>% complete(variable = i, last_value = last_row_simulation[[i]], steadystate = SS_val_computed)
    }
    aux <- aux %>% drop_na()
    aux <- aux %>% mutate_at(vars(steadystate, last_value), round, digits = 2)
    aux <- aux %>% mutate(is_same = case_when(
      last_value == steadystate ~ "Equal",
      TRUE ~ "Different"
    ))
    aux <- aux %>% rename("Theoretic Value" = steadystate,
                          "Simulated Value" = last_value,
                          "Variable" = variable)
    return(aux)

}


# 0.9 compare different simulations =================================
compare_simulations <- function(simulation_list, sim_identifier_vector, vars_selection){

  # Roxygen Header ---------------------------------
  #' @title (Superseeded!) Visualise Common Variables of Different Solow Models
  #' @description Visualise the evolution of common variables of multiple (2 or more) Solow variants in the same graph.
  #' @details
  #' @param simulation_list List with tibbles. The tibbles need to be the results of the simulationfunctions, e.g. \code{SimulateBasicSolowModel()}.
  #' @param sim_identifier_vector Vector with the model codes. (The first element of \code{sim_identifier_vector} should correspond to the first element of \code{simulation_list}.)
  #' @param vars_selection Vector with the abbreviated variable names to visualise. (This vector can of course only contain variable abbreviations that are shared by all the simulation in \code{simulation_list}.)
  #' @note To see which variables can be selected, that is, to see which variables are common to two models, can be seen with \code{getVariablesAvailableToBeVisualised()}.
  #' @examples
  #' \dontrun{compare_simulations( list(SimulateBasicSolowModel(), SimulateGeneralSolowModel()), c("BS", "GS"), c("Y", "K", "L", "WR", "RR"))}
  #' @export

  # Function ---------------------------------
  # simulation_list is list(sim1, sim2, sim3, ...)
  # sim_identifier_vector is c("oil", "land", "oilland")
  # vars_selection is a vector of the variables to plot.

  # Verifying that inputs are 'correct' and can be worked with
  if(length(simulation_list) != length(sim_identifier_vector)){
    stop("Number of simulation identifier strings and number of simulations don't match.")
  }

  for(i in seq_along(simulation_list)){
   simulation_list[[i]] <- simulation_list[[i]] %>% mutate(sim_type = sim_identifier_vector[[i]])
  }

  if("period" %in% vars_selection){
    stop("You supplied the 'period' column into your variable selection. Remove it from the selection, please.")
  }
  if("sim_type" %in% vars_selection){
    stop("You supplied the 'sim_type' column into your variable selection. Remove it from the selection, please.")
  }
  # generating cumuative intersections of all column names.
  # only those that exist in all simulations can be plotted in a comparison plot of variables across all simulations.
  list_of_col_names <- map(simulation_list, names)
  col_names_shared <- list_of_col_names[[1]]
  for(i in c(2:length(list_of_col_names))){
    col_names_shared <- intersect(col_names_shared, list_of_col_names[[i]])
  }
  # stacking all simulations with the shared column names
  sims_stacked <- simulation_list[[1]] %>% select(all_of(col_names_shared)) %>% mutate(sim_type = sim_identifier_vector[[1]])
  for(i in c(2:length(simulation_list))){
    # i <- 2
    sims_stacked <- sims_stacked %>% bind_rows(
      simulation_list[[i]] %>%
        select(all_of(col_names_shared)) %>%
        mutate(sim_type = sim_identifier_vector[[i]])
      )
  }

  library(tidyverse)

  theme_set(
    theme_classic() +
      theme(
        axis.ticks.length = unit(-0.25, "cm"),
        axis.text.x = element_text(margin = unit(c(0.4,0,0,0), "cm")),
        axis.text.y = element_text(margin = unit(c(0,0.4,0,0), "cm")),
        axis.line = element_blank(),
        panel.grid.major.y = element_line(linetype = 2),
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "serif"),
        legend.justification = c("right", "top"),
        # legend.position = c(1, 1),
        legend.position = c(.98, .98),
        legend.background = element_rect(fill = NA, color = "black"),
        panel.border = element_rect(fill = NA, size = 1.25),
        strip.text = element_text(size = 12)
        # legend.margin = margin(6, 10, 6, 6)
        # legend.box.background = element_rect(colour = "black")
      )

  )


  sims_stacked %>%
    select(all_of(c("period", "sim_type", vars_selection))) %>%
    pivot_longer(-c("period", "sim_type"), names_to = "Variable") %>%
    mutate(Variable = as.factor(Variable)) %>%
    ggplot(aes(period, value, col = sim_type, group = sim_type)) +
    geom_line(alpha = 0.75) +
    facet_wrap(~Variable, scales = "free", ncol = 2) +
    labs(x = "Period", y = "Value", col = "Solow Variant")
}

# 0.10 get the variables for which starting values need to be filled for a specific Solow variant =================================

# Disclaimer: I am aware of the mathematically completely outrageous enumeration of this and the
# following sections. I need however to keep the R headers starting at 0 for the helper functions.
# Would feel wrong not to do so.

getRequiredStartingValues <- function(ModelCode){

  # Roxygen Header ---------------------------------
  #' @title Get the variables that need starting values for t = 0 of the Solow variant
  #' @description Each simulation function requires certain key endogenous variables to be
  #' initialised at \eqn{t = 0}. This function yields precisely those variables
  #' (resp. their abbreviations).
  #' @param ModelCode Model abbreviation, such as "BS", "GS", "ESHC", ESSRO". (See the vignette to this package for an exhaustive listing of possible model codes.)
  #' @examples getRequiredStartingValues("ESSOE")
  #' @export

  # Function ---------------------------------
  out <- if (ModelCode == "BS") {
    out <- c("K", "L")
  } else if (ModelCode == "GS") {
    out <- c("A", "K", "L")
  } else if (ModelCode == "ESSOE") {
    out <- c("L", "V")
  } else if (ModelCode == "ESSRL") {
    out <- c("A", "K", "L")
  } else if (ModelCode == "ESSRO") {
    out <- c("A", "K", "L", "R")
  } else if (ModelCode == "ESSROL") {
    out <- c("A", "K", "L", "R")
  } else if (ModelCode == "ESHC") {
    out <- c("A", "K", "L", "H")
  } else {
    (
      out <- NaN
    )
  }
  if (is.na(out[[1]])) {
    warning("The entered shortcode for a model variant does not exist.")
  }
  return(out)
}


getRequiredParams <- function(ModelCode) {

  # Roxygen Header ---------------------------------
  #' @title Get the parameters that define a certain Solow variant
  #' @description Each simulation function requires certain parameters to be
  #' set. This function yields precisely those parameters
  #' (resp. their abbreviations).
  #' @param ModelCode Model abbreviation, such as "BS", "GS", "ESHC", ESSRO". (See the vignette to this package for an exhaustive listing of possible model codes.)
  #' @examples getRequiredStartingValues("BS")
  #' @export

  # Function ---------------------------------

  out <- if (ModelCode == "BS") {
    out <- c("B", "alpha", "delta", "n", "s")
  } else if (ModelCode == "GS") {
    out <- c("g", "alpha", "delta", "n", "s")
  } else if (ModelCode == "ESSOE") {
    out <- c("B", "alpha", "n", "s", "r")
  } else if (ModelCode == "ESSRL") {
    out <- c("alpha", "beta", "delta", "n", "s", "g", "X")
  } else if (ModelCode == "ESSRO") {
    out <- c("alpha", "beta", "n", "g", "sE", "s", "delta")
  } else if (ModelCode == "ESSROL") {
    out <- c("alpha", "beta", "kappa", "delta", "n", "s", "sE", "g", "X")
  } else if (ModelCode == "ESHC") {
    out <- c("alpha", "phi", "n", "g", "sK", "sH", "delta")
  } else {
    (
      out <- NaN
    )
  }
  if (is.na(out[[1]])) {
    warning("The entered shortcode for a model variant does not exist.")
  }
  return(out)
}

getVariablesAvailableToBeVisualised <- function(ModelCode1,
                                                ModelCode2){

  # Roxygen Header ---------------------------------
  #' @title to be filled
  #' @description tbd
  #' @details tbd
  #' @param ModelCode1 Model abbreviation for some Solow variant.
  #' @param ModelCode2 Model abbreviation for some Solow variant.
  #' @examples getVariablesAvailableToBeVisualised("BS", "ESSRO")
  #' @export

  # Function ---------------------------------
  sourceSimulationFile(ModelCode1)
  sourceSimulationFile(ModelCode2)

  variables1 <- get(paste0("meta_", ModelCode1, "_variables"))
  variables2 <- get(paste0("meta_", ModelCode2, "_variables"))
  shared_variables <- intersect(variables1, variables2)
  return(shared_variables)
}

# 0.99 testing =================================
# vtstest <- c("testvar", "ja", "nein")
# create_simulation_table(vtstest, 20)
# variable_encoder(c("Rental Rate"))
# add_var_computer(tibble(L = 3, Y = 9, YpW = NA), c(T, T, F), c(), "exo", "BS")

# last_row_simulation <- testsimulation[nrow(testsimulation), ]
# last_row_parameter <- paragrid[nrow(paragrid), ]
# solow_variant <- "BS"
#
# result <- simulation_correctness_checker(testsimulation[nrow(testsimulation), ],
#                                paragrid[nrow(paragrid), ],
#                                "BS")

# result$last_value
# result$steadystate

# simulation_list <- list(testsimulation_general, testsimulation_land)
# sim_identifier_vector <- c("General Solow Model", "Extended Solow Model with Scarce Resources --- Land")
# vars_selection <- names(testsimulation_general)[c(4, 6, 18, 23)]
# compare_simulations(list(testsimulation_general, testsimulation_land),
#                     c("General Solow Model", "Extended Solow Model with Scarce Resources --- Land"),
#                     names(testsimulation_general)[c(4, 6, 19, 23)])
