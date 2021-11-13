# This file sets up several helper functions that will be used throughout the Solow model simulation functions.

# General Imports of other Packages =================================
#' @import R.utils
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import purrr

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
create_parameter_grid <- function(names, ivs, pfcs, nvs, np){

  # Roxygen Header ---------------------------------
  #' @title Create a set of parameter paths (grid).
  #' @description Creates dataframe with \code{np+1} rows and
  #' \code{length(names)} columns.
  #' @details The word "grid" refers to multiple parameter paths.
  #' The term will be referenced throughout the simulation functions
  #' as every model is defined via exogenously given parameters that will
  #' be supplied and referenced as a 'parameter-grid' in the simulation functions.
  #' **Warning**: Make sure you use the correct names in \code{names} for the respective parameters. Use \code{getRequiredParams(ModelCode)} to get them. The model abbreviations for \code{ModelCode} (used consistently throughout the package) can be found in the vignette.
  #' **Warning**: The inputs will be used to create the paramter paths. For each paramter, the same index will be used across all input vectors. That means that the first entry to \code{ivs} will be the initial value to the parameter that is the first entry in the \code{names} vector. Same applies the rest of the inputs to this function.
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

  # Verifying appropriate inputs
  lengths_of_inputs <- c(length(names), length(ivs), length(pfcs), length(nvs))
  if(min(lengths_of_inputs) != max(lengths_of_inputs)){
    stop("The inputs to create_parameter_grid are of different lengths. They need to contain the same number of entries.")
  }
  # Function ---------------------------------
  aux <- tibble(period = c(0:np))
  for(i in seq_along(names)){
    aux_path <- create_path(ivs[[i]], pfcs[[i]], nvs[[i]], np)
    aux[[names[[i]] ]] <- aux_path
  }
  return(aux)
}



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
      aux2 == "Growth Rate of Total Factor Productivity" ~ "gTFP",

      aux2 == "Human Capital Stock" ~ "H",
      aux2 == "Log of Human Capital Stock" ~ "logH",
      aux2 == "Capital Stock" ~ "K",
      aux2 == "Physical Capital Stock" ~ "K", # no interference here "Physical ..." only used in ESHC where the usual Capital STock is ambiguous.
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
      aux2 == "National Output per Worker" ~ "YnpW",
      aux2 == "National Wealth" ~ "V",
      aux2 == "National Wealth per Worker"~ "VpW",
      aux2 == "National Wealth per Effective Worker"~ "VpEW",
      aux2 == "Net Foreign Assets" ~ "F",
      aux2 == "Net Foreign Assets per Worker" ~ "FpW",
      aux2 == "National Savings" ~ "Sn",

      aux2 == "Energy Use" ~ "E",
      aux2 == "Resource Stock" ~ "R",

      aux2 == "Consumption" ~ "C",
      aux2 == "Consumption per Worker" ~ "CpW",
      aux2 == "Consumption per Effective Worker" ~ "CpEW",

      aux2 == "Capital to Output Ratio" ~ "CtO", 
      aux2 == "Physical Capital to Output Ratio" ~ "CtO", 
      # the two above don't interfere with each other teh Physical Capital is onyl used in ESHC where the usual Capital to Output Ratio is not used.
      aux2 == "Human Capital to Output Ratio" ~ "HCtO"



    )
    aux[[i]] <- aux3

  }
  return(aux)
}


# 0.6 visualise a simulation =================================
VisualiseSimulation <- function(simulation_data, variables, scale_identifier = "free"){

  # Roxygen Header ---------------------------------
  #' @title Visualise selected variables of a simulation
  #' @param simulation_data The output tibble of any simulation function.
  #' @param variables The variables to visualise in *abbreviated* form.
  #' @param scale_identifier string to indicate freely floating scales ("free") or fixed sales ("fixed")
  #' @export

  # Function ---------------------------------

  set_default_theme()
  
  for(i in variables){
    if(i %in% names(simulation_data)){
    }else{
      aux_error_message <- 
        paste("The variable", 
              i, 
              "is not endogenous to the simulation entered into the function VisualiseSimulation and thus, cannot be visualised.")
      stop(aux_error_message)
    }
  }
  variables <- c("period", variables)
  simulation_data %>% select(all_of(variables)) %>%
    pivot_longer(-period, names_to = "Variable") %>%
    mutate(Variable = as.factor(Variable)) %>%
    ggplot(aes(period, value, col = Variable)) +
    geom_line() +
    facet_wrap(~factor(Variable, levels = variables), scales = scale_identifier, ncol = 2)+
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
  #' @param technology_variant A string indicating the exogenous (\code{technology_variant = "exo"}) or endogenous (\code{technology_variant = "endo"}) nature of technology in the respective model. A special case (\code{technology_variant = "special"}) stands for the technology form(s) of endogenous technology growth models (model code "ESEG").
  #' @param solowversion The model code for the model, such as "BS" or "ESSOE". (The same variables such as WR or RR are computed differently depending on the model.)
  #' @note See SimulateBasicSolowModel() for an example.
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
    technology <- rep(NA, dim(sim_data)[1])
    # A is defined differnetly in semi-endo growth variant and fully-endo variant of the ESEG variant
    # semi: it^s K^phi
    # fully: it's L^(1-alpha) where L is actualy constant (somewhat paramter)
    for (i in seq_along(technology)){
      if(parameter_data[["phi"]][[i]] < 0.95){
        technology[[i]] <- sim_data[["K"]][[i]]^(parameter_data[["phi"]][[i]])
      }else if(parameter_data[["phi"]][[i]] %>% between(0.95, 1)){
        technology[[i]] <- sim_data[["L"]][[i]]^(1 - parameter_data[["alpha"]][[i]])
    }
  }
  }else{
    stop("Technology location unclear. Fill in an appropriate value for technology_variant. ")
  }
  # second: computing and filling in the secondary endogenous variables
  for(i in names(sim_data)[!add_vars]){
    # Variants of Output
    if(i == "YpW"){sim_data[["YpW"]] <- sim_data[["Y"]]/sim_data[["L"]]}
    if(i == "YpEW"){sim_data["YpEW"] <- sim_data[["Y"]]/(technology * sim_data[["L"]])}
    if(i == "YnpW"){sim_data["YnpW"] <- sim_data[["Yn"]]/sim_data[["L"]]}
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
    if(i == "gTFP"){sim_data[["gTFP"]] <- log(sim_data[["TFP"]]) - log(lag(sim_data[["TFP"]]))}
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
    if(i == "VpEW"){sim_data[["VpEW"]] <- sim_data[["V"]] / (sim_data[["L"]]*technology)}
    if(i == "FpW"){sim_data[["FpW"]] <- sim_data[["F"]] / sim_data[["L"]]}
    # Variants of Consumption
    if(solowversion != "ESHC"){
    if(i == "C"){sim_data[["C"]] <- sim_data[["Y"]] * (1- parameter_data[["s"]])}
      }
    if(i == "CpW"){sim_data[["CpW"]] <- sim_data[["C"]] / sim_data[["L"]]}
    if(i == "CpEW"){sim_data[["CpEW"]] <- sim_data[["C"]] / (technology * sim_data[["L"]])}

    if(i == "CtO"){sim_data[["CtO"]] <- sim_data[["K"]]/sim_data[["Y"]]}
    if(i == "HCtO"){sim_data[["HCtO"]] <- sim_data[["H"]]/sim_data[["Y"]]}


    # Variables uniquely calculated to different Solow Model Versions (e.g. WR, RR)
    # WR, RR for BS ---------------------------------
    if(solowversion == "BS") {
      # source("ModelFunctions/BSModelFunctions.R")
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
      # source("ModelFunctions/GSModelFunctions.R")
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
        # source("ModelFunctions/ESSOEModelFunctions.R")
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
          # source("ModelFunctions/ESHCModelFunctions.R")
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
        
        if(i == "C"){sim_data[["C"]] <- (1- (parameter_data[["sK"]] + parameter_data[["sH"]])) * sim_data[["Y"]]}




      }
    # WR, RR for ESSRL ---------------------------------
    if(solowversion == "ESSRL") {
        # source("ModelFunctions/ESSRLModelFunctions.R")
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
        # source("ModelFunctions/ESSROModelFunctions.R")
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
        # source("ModelFunctions/ESEGModelFunctions.R")
      if(i == "TFP"){
        for (i in seq_along(sim_data[["TFP"]])){
          if(parameter_data[["phi"]][[i]] < 0.95){
            sim_data[["TFP"]][[i]] <- sim_data[["K"]][[i]]^(parameter_data[["phi"]][[i]])
          }else if(parameter_data[["phi"]][[i]] %>% between(0.95, 1)){
            sim_data[["TFP"]][[i]] <- sim_data[["L"]][[i]]^(1 - parameter_data[["alpha"]][[i]])
          }
        } 
      }
      if (i == "WR") {
        sim_data[["WR"]] <- ESEG_MF_WR(technology,
                                       sim_data[["K"]],
                                       sim_data[["L"]],
                                       parameter_data[["alpha"]],
                                       parameter_data[["phi"]])
        
      }
      # Rental Rate
      if (i == "RR") {
        sim_data[["RR"]] <- ESEG_MF_RR(technology,
                                       sim_data[["K"]],
                                       sim_data[["L"]],
                                       parameter_data[["alpha"]],
                                       parameter_data[["phi"]])
      }




    }



  }

  return(sim_data)
}

# 0.8 compute steady state values and validate the simulation against the theoretically to be attained values with the current model setup (paramters, starting values) =================================
steadystate_checker <- function(sim_data, parameter_grid, solow_variant){

  # Roxygen Header ---------------------------------
  #' @title Check correctness by comparing simulated (endo.) variables to their steady state values
  #' @description Compare variables in the final period of the simulation to their respective steady state value (given the exo. paramters).
  #' @details This function presumes that the number of periods simulated were such that the final values are near steady state. If the steady state path is disrupted by a parameter change in the second to last period, this function will yield misleading results. 
  #' @note The column "Economy in SS" is evaluated as TRUE when the simulated value is within a .1\% interval around the steady state value for reason of computational inacurracy.
  #' @param sim_data The tibble that is being filled in the simulation function, e.g. \code{SimulateBasicSolowModel()}.
  #' @param parameter_grid The output from \code{create_parameter_grid(...)}.
  #' @param solow_variant String indicating the model, such as "BS" or "ESHC".
  #' @export

  # Function ---------------------------------
    last_row_simulation <- sim_data[nrow(sim_data), ]
    last_row_parameter <- parameter_grid[nrow(parameter_grid), ]

    aux <- tibble(variable = toString(NA), last_value = as.double(NA), steadystate = as.double(NA))
    aux[[1,1]] <- NA

    all_possible_steady_state_function_inputs <-
      list(
        delta = last_row_parameter[["delta"]],
        s = last_row_parameter[["s"]],
        sK = last_row_parameter[["sK"]],
        sH = last_row_parameter[["sH"]],
        sE = last_row_parameter[["sE"]],
        sR = last_row_parameter[["sR"]],
        n = last_row_parameter[["n"]],
        B = last_row_parameter[["B"]],
        r = last_row_parameter[["r"]],
        rho = last_row_parameter[["rho"]],
        g = last_row_parameter[["g"]],
        alpha = last_row_parameter[["alpha"]],
        beta = last_row_parameter[["beta"]],
        kappa = last_row_parameter[["kappa"]],
        lambda = last_row_parameter[["lambda"]],
        phi = last_row_parameter[["phi"]],
        YpW = last_row_simulation[["YpW"]],
        RR = last_row_simulation[["RR"]],
        w = last_row_simulation[["WR"]],
        A = last_row_simulation[["TFP"]],
        KpW = last_row_simulation[["KpW"]],
        L = last_row_simulation[["L"]],
        X = last_row_parameter[["X"]],
        K = last_row_simulation[["K"]],
        k = last_row_parameter[["k"]],
        R = last_row_simulation[["R"]],
        E = last_row_parameter[["sE"]]* last_row_simulation[["R"]]
      )

    if(solow_variant == "BS") {
      # Remark: The selections variable_encoder(meta_GS_variables[c(6, 7, 8, 9)]) can be generally adjusted to simply c("KpW", "YpW", ...) as done for some
      aux_steadystate_variables <- c("KpW", "YpW", "CpW", "WR", "RR")
    }else if(solow_variant == "GS"){
      aux_steadystate_variables <- c("KpW", "YpW", "CpW", "WR", "RR", "KpEW", "YpEW")
    }else if(solow_variant == "ESSOE"){
      aux_steadystate_variables <- c("KpW", "YpW", "CtO", "YnpW", "WR", "VpW", "FpW")
    }else if(solow_variant == "ESHC"){
      aux_steadystate_variables <- c("KpEW", "HpEW", "YpEW", "YpW", "CpW") # WR and RR missing
    }else if(solow_variant == "ESSRL"){
      aux_steadystate_variables <- c("CtO", "YpW") # WR and RR missing
    }else if(solow_variant == "ESSRO"){
      aux_steadystate_variables <- c("YpW")
    }else if(solow_variant == "ESSROL"){
      aux_steadystate_variables <- c("gY", "YpW")
    }else if(solow_variant == "ESEG"){
      if(last_row_parameter[["phi"]] < 0.95){
      aux_steadystate_variables <- c("KpEW", "YpEW", "gYpW")
      }else if(last_row_parameter[["phi"]] %>% between(0.95, 1)){
      aux_steadystate_variables <- c("gYpW")
      }
    }else if(solow_variant == "ESEGRomer"){
      if(last_row_parameter[["phi"]] < 0.95){
      aux_steadystate_variables <- c("YpEW", "gYpW") #...SS_gYpW would exist, but is yielding correct results.
      }else if(last_row_parameter[["phi"]] %>% between(0.95, 1)){
      aux_steadystate_variables <- c("YpEW")
      }
    }else if(solow_variant == "ESEGCozziOne"){
      if(last_row_parameter[["phi"]] < 0.95){
      aux_steadystate_variables <- c("KpEW", "YpEW", "gYpW")
      }else if(last_row_parameter[["phi"]] %>% between(0.95, 1)){
      aux_steadystate_variables <- c("gYpW")
      }
    }else if(solow_variant == "ESEGCozziTwo"){
      aux_steadystate_variables <- c("gTFP", "KpEW", "YpEW")
    }

    for(i in aux_steadystate_variables){
      aux_function_name <- paste(solow_variant, "_SS_", i, sep = "")
      SS_val_computed <- doCall(aux_function_name, args = all_possible_steady_state_function_inputs )
      aux <- aux %>% complete(variable = i, last_value = last_row_simulation[[i]], steadystate = SS_val_computed)
    }
    aux <- aux %>% drop_na()
    aux <- aux %>% mutate_at(vars(steadystate, last_value), round, digits = 2)
    aux <- aux %>% mutate(is_same = case_when(
      abs(last_value - steadystate) <= 0.01 ~ "TRUE",
      TRUE ~ "FALSE"
    ))
    aux <- aux %>% rename("Theoretical Value" = steadystate,
                          "Simulated Value" = last_value,
                          "Variable" = variable,
                          "Economy in SS" = is_same)
    return(aux)
    message("Remark: steadystate_checker considers two values 0.01 away from each other 'equal' because of inaccuracy from floating point point calculations.")

}


# 0.9 compare different simulations =================================
compare_simulations <- function(simulation_list, sim_identifier_vector, vars_selection){

  # Roxygen Header ---------------------------------
  #' @title Visualise Common Variables of Different Solow Models
  #' @description Visualise the evolution of common variables of multiple (2 or more) Solow variants in the same graph.
  #' @param simulation_list List with tibbles. The tibbles need to be the results of the simulationfunctions, e.g. \code{SimulateBasicSolowModel()}.
  #' @param sim_identifier_vector Vector with the model codes. (The first element of \code{sim_identifier_vector} should correspond to the first element of \code{simulation_list}.)
  #' @param vars_selection Vector with the abbreviated variable names to visualise. (This vector can of course only contain variable abbreviations that are shared by all the simulation in \code{simulation_list}.)
  #' @note To see which variables can be selected, that is, to see which variables are common to two models, can be seen with \code{getVariablesAvailableToBeVisualised()}.
  #' @examples
  #' \dontrun{compare_simulations( list(SimulateBasicSolowModel(), SimulateGeneralSolowModel()), c("BS", "GS"), c("Y", "K", "L", "WR", "RR"))}
  #' @export

  # Function ---------------------------------

  set_default_theme()

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

  sims_stacked %>%
    select(all_of(c("period", "sim_type", vars_selection))) %>%
    relocate(vars_selection) %>% 
    pivot_longer(-c("period", "sim_type"), names_to = "Variable") %>%
    mutate(Variable = as.factor(Variable)) %>%
    ggplot(aes(period, value, col = sim_type, group = sim_type)) +
    geom_line(alpha = 0.75) +
    facet_wrap(~Variable, scales = "free", ncol = 2) +
    labs(x = "Period", y = "Value", col = "Solow Variant", title = paste("Comparison between Variants", sim_identifier_vector[[1]], "and", sim_identifier_vector[[2]])) + 
    theme(legend.position = "bottom", legend.justification = "center")
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
  } else if (ModelCode == "ESEG") {
    out <- c("K", "L")
  } else if (ModelCode == "ESEGRomer") {
    out <- c("A", "K", "L")
  } else if (ModelCode == "ESEGCozziOne") {
    out <- c("A", "K", "L")
  } else if (ModelCode == "ESEGCozziTwo") {
    out <- c("A", "K", "L")
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

# 0.11 get the parameters for which starting values need to be filled for a specific Solow variant =================================

getRequiredParams <- function(ModelCode) {

  # Roxygen Header ---------------------------------
  #' @title Get the parameters that define a certain Solow variant
  #' @description Each simulation function requires certain parameters to be
  #' set. This function yields precisely those parameters
  #' (resp. their abbreviations).
  #' @param ModelCode Model abbreviation, such as "BS", "GS", "ESHC", ESSRO". (See the vignette to this package for an exhaustive listing of possible model codes.)
  #' @examples getRequiredParams("BS")
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
  } else if (ModelCode == "ESEG") {
    out <- c("alpha", "phi", "s", "delta", "n")
  } else if (ModelCode == "ESEGRomer") {
    out <- c("alpha", "phi", "lambda", "rho", "s", "sR", "delta", "n")
  } else if (ModelCode == "ESEGCozziOne") {
    out <- c("alpha", "phi", "lambda", "rho", "s", "sR", "delta", "n")
  } else if (ModelCode == "ESEGCozziTwo") {
    out <- c("alpha", "phi", "lambda", "rho", "s", "sR", "delta", "n", "k")
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

# 0.12 get the inputs required for the transition equation for a specific Solow variant =================================
getRequiredInputsToTE<- function(ModelCode) {

  # Roxygen Header ---------------------------------
  #' @title Get the inputs that are required to draw a models transition diagram (with the transition equation)
  #' @param ModelCode Model abbreviation, such as "BS", "GS", "ESHC", ESSRO". (See the vignette to this package for an exhaustive listing of possible model codes.)
  #' @examples getRequiredInputsToTE("BS")
  #' @export

  # Function ---------------------------------

  out <- if (ModelCode == "BS") {
    out <- c("B", "alpha", "delta", "n", "s")
  } else if (ModelCode == "GS_XXXXX") {
    out <- c("g", "alpha", "delta", "n", "s")
  } else if (ModelCode == "ESHC_XXXXX") {
    out <- c("alpha", "phi", "n", "g", "sK", "sH", "delta")
  } else if (ModelCode == "ESSOE") {
    out <- c("B", "alpha", "n", "s", "r", "w")
  } else if (ModelCode == "ESSRL_XXXXX") {
    out <- c("alpha", "beta", "delta", "n", "s", "g", "X")
  } else if (ModelCode == "ESSRO_XXXXX") {
    out <- c("alpha", "beta", "n", "g", "sE", "s", "delta")
  } else if (ModelCode == "ESSROL_XXXXX") {
    out <- c("alpha", "beta", "kappa", "delta", "n", "s", "sE", "g", "X")
  } else if (ModelCode == "ESEG_XXXXX") {
    out <- c("alpha", "phi", "n", "g", "sK", "sH", "delta")
  } else if (ModelCode == "ESEGRomer_XXXXX") {
    out <- c("alpha", "phi", "n", "g", "sK", "sH", "delta")
  } else if (ModelCode == "ESEGCozziOne_XXXXX") {
    out <- c("alpha", "phi", "n", "g", "sK", "sH", "delta")
  } else if (ModelCode == "ESEGCozziTwo_XXXXX") {
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

draw_transition_diagram <- function(paragrid_special, solow_variant){

  # Roxygen Header ---------------------------------
  #' @title Draw the transition diagram for a solow variant
  #' @param paragrid_special A special parameter grid. See the examples below to see how this input is supposed to look like.
  #' @param solow_variant A model code. Currently only "BS", "GS" and "ESSOE" supported.
  #' @examples
  #' draw_transition_diagram(
  #' list(B = c(1),
  #'      alpha = 1/3, 
  #'      delta = 0.2, 
  #'      n = 0.005, 
  #'      s = c(0.25, 0.05)
  #' ),
  #' "BS"
  #' )
  #' @export

  # Function ---------------------------------  
  # Create string with function call to the XX_TE
  aux_TE_name <- paste0(solow_variant, "_TE")
  # Catch underspecifications in the parameters
  if(any(!(names(paragrid_special) %in% getRequiredInputsToTE(solow_variant)))){
    stop("The parameter paragrid_special does not include all required parameters.
         Check getRequiredInputsToTE() to see which parameter is missing.")
  }
  
  # Set up the parameter lists for the two lines in the transition diagram
  aux_parameter_list_template <- list()
  aux_parameter_list_1 <- aux_parameter_list_template
  aux_parameter_list_2 <- aux_parameter_list_template
  
  # Fill in the parameter lists
  for (i in names(paragrid_special)){
    if(length(paragrid_special[[i]]) == 1){
      aux_parameter_list_1[[i]] <- paragrid_special[[i]]
      aux_parameter_list_2[[i]] <- paragrid_special[[i]]
    }else{
      aux_parameter_list_1[[i]] <- paragrid_special[[i]][1]
      aux_parameter_list_2[[i]] <- paragrid_special[[i]][2]
    }
  }
  
  # get variables to be plotted in TE (differing with solow models)
  TE_var <- getTDAxes(solow_variant)
  aux_SS_function <- paste0(solow_variant, "_SS_", TE_var)
  # get steady states under the different sets of parameters to determine the x-axis length
  aux_SS_paraset1 <- doCall(aux_SS_function, args = aux_parameter_list_1)
  aux_SS_paraset2 <- doCall(aux_SS_function, args = aux_parameter_list_2)
  if(aux_SS_paraset1 > aux_SS_paraset2){
    aux_x_limit <- aux_SS_paraset1
  }else{
    aux_x_limit <- aux_SS_paraset2
  }
  # initialise output table
  aux_df <- tibble(x = seq(0.0001,aux_x_limit *1.3, aux_x_limit/5000))
  
  aux_parameter_list_1[["x"]] <- aux_df$x
  aux_parameter_list_2[["x"]] <- aux_df$x
  # add the lines of the tranistion equation to the output table
  aux_df["TE1"] <- doCall(aux_TE_name, args = aux_parameter_list_1)
  aux_df["TE2"] <- doCall(aux_TE_name, args = aux_parameter_list_2)
  # visualise
  ggplot(aux_df) +
    geom_line(aes(x, TE1), size = 0.3) +
    geom_line(aes(x, TE2), size = 0.3) +
    geom_abline(intercept = 0, slope = 1, lty = 2, size = 0.3) +
    labs(title = paste("Transition Diagram of the", solow_variant),
         x = "KpW_{t}", y = "KpW_{t+1}",
         caption = "The dashed line represents KpW_{t} = KpW_{t+1}.")
}



getTDAxes <- function(ModelCode){
  out <- if (ModelCode == "BS") {
    out <- c("KpW")
  } else if (ModelCode == "GS") {
    out <- c(NA)
  } else if (ModelCode == "ESSOE") {
    out <- c("VpW")
  } else if (ModelCode == "ESSRL") {
    out <- c(NA)
  } else if (ModelCode == "ESSRO") {
    out <- c(NA)
  } else if (ModelCode == "ESSROL") {
    out <- c(NA)
  } else if (ModelCode == "ESHC") {
    out <- c(NA)
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

eval_string <- function(string){eval(parse(text = string))}

getModelVars <- function(ModelCode){
  #' @export
  aux_add_vars <- paste0("add_vars_", ModelCode)
  out <- c(base_variables, eval_string(aux_add_vars))
  return(out)
}

set_default_theme <- function(){
  #' @export 
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
}


getCommonParameters <- function(ModelCode1, ModelCode2){

  # Roxygen Header ---------------------------------
  #' @title Yields the union of required parameters for two solow variants
  #' @param ModelCode1 Model abbreviation for some Solow variant.
  #' @param ModelCode2 Model abbreviation for some Solow variant.
  #' @examples
  #' getUnifiedParameters("BS", "ESSOE")
  #' @export

  # Function ---------------------------------
  aux1 <- getRequiredParams(ModelCode1)
  aux2 <- getRequiredParams(ModelCode2)

  unified <- union(aux1, aux2)
  return(unified)
}

getCommonStartingValues <- function(ModelCode1, ModelCode2){

  # Roxygen Header ---------------------------------
  #' @title Yields the union of required starting values for two solow variants
  #' @param ModelCode1 Model abbreviation for some Solow variant.
  #' @param ModelCode2 Model abbreviation for some Solow variant.
  #' @examples
  #' getunifiedstartingvalues("BS", "ESSOE")
  #' @export

  # Function ---------------------------------
  aux1 <- getRequiredStartingValues(ModelCode1)
  aux2 <- getRequiredStartingValues(ModelCode2)

  common_values <- union(aux1, aux2)
  
  return(common_values) 
}

plausible_parameter_entries <- function(parameters, aux_list = NULL){

  # Roxygen Header ---------------------------------
  #' @export

  # Function ---------------------------------
  out <- c()
  specified_parameters <- names(aux_list)
  for(i in parameters){
    if(i %in% specified_parameters){
      aux <- aux_list[[i]]
    }else{
      aux <- case_when(
        i == "B" ~ 1, 
        i == "alpha" ~ 0.25, 
        i == "delta" ~ 0.2, 
        i == "n" ~ 0.005, 
        i == "s" ~ 0.22, 
        i == "g" ~ 0.02, 
        i == "r" ~ 0.03, 
        i == "phi" ~ 0.25, 
        i == "sK" ~ 0.2, 
        i == "sH" ~ 0.2, 
        i == "beta" ~ 0.25, 
        i == "sE" ~0.02, 
        i == "X" ~ 1, 
        i == "kappa" ~ 0.25, 
        i == "lambda" ~ 0.2, 
        i == "rho" ~ 0.1, 
        i == "sR" ~ 0.2, 
        i == "k" ~ 0.3
    )
    }
    out <- c(out, aux)
  }
  return(out)
}

plausible_startingvalue_entries <- function(variables, aux_list = NULL){
  
  # Roxygen Header ---------------------------------
  #' @export
  
  # Function ---------------------------------
  out <- list()
  specified_startingvalue <- names(aux_list)
  for(i in variables){
    if(i %in% specified_startingvalue){
      out[[i]] <- aux_list[[i]]
    }else{
      out[[i]] <- case_when(
        i == "H" ~ 1,
        i == "K" ~ 1,
        i == "L" ~ 1,
        i == "A" ~ 1,
        i == "V" ~ 1,
        i == "R" ~ 1
      )
    }
  }
  return(out)
}

set_all_na <- function(vector){

  # Roxygen Header ---------------------------------
  #' @export

  # Function ---------------------------------
  rep(NA, length(vector))
}

getSimFunction <- function(ModelCode) {
  #' @export
  out <- case_when(
    ModelCode == "BS" ~ "SimulateBasicSolowModel",
    ModelCode == "GS" ~ "SimulateGeneralSolowModel",
    ModelCode == "ESHC" ~ "SimulateExtendedSolowModelHumanCapital",
    ModelCode == "ESSOE" ~ "SimulateExtendedSolowModelSmallOpenEconomy",
    ModelCode == "ESSRO" ~ "SimulateExtendedSolowModelScarceResourceOil",
    ModelCode == "ESSRL" ~ "SimulateExtendedSolowModelScarceResourceLand",
    ModelCode == "ESSROL" ~ "SimulateExtendedSolowModelScarceResourceOilAndLand",
    ModelCode == "ESEG" ~ "SimulateExtendedSolowModelEndogenousGrowth",
    ModelCode == "ESEGRomer" ~ "SimulateExtendedSolowModelEndogenousGrowthRomer",
    ModelCode == "ESEGCozziOne" ~ "SimulateExtendedSolowModelEndogenousGrowthCozziOne",
    ModelCode == "ESEGCozziTwo" ~ "SimulateExtendedSolowModelEndogenousGrowthCozziTwo",
    TRUE ~ "NaN"
  )
  if (out == "NaN") {
    warning("The entered shortcode for a model variant does not exist.")
  }
  return(out)
}


########################## kept but not used ##########################
#' 
#' draw_transition_diagram <- function(sim_data, parameter_grid, solow_variant){
#'   # sim_data <- aux_simulation
#'   # parameter_grid <- aux_parameter_grid
#'   # solow_variant <- "BS"
#'   #' @title
#'   #' @description
#'   #' @inheritParams steadystate_checker
#'   
#'   
#'   theme_set(theme_bw())
#'   ggplot(aux_out) + geom_line(aes(x, y)) + geom_abline(intercept = 0, slope = 1) + labs(y = "KpW_{t + 1}", x = "KpW_{t}")
#'   if(solow_variant == "BS"){
#'     i_SS <- dim(sim_data)[1] # usually the last  
#'     SS_value <- BS_SS_KpW(parameter_grid[["B"]][i_SS], 
#'                           parameter_grid[["alpha"]][i_SS], 
#'                           parameter_grid[["s"]][i_SS], 
#'                           parameter_grid[["n"]][i_SS], 
#'                           parameter_grid[["delta"]][i_SS])
#'     aux_out <- 
#'         tibble(x = seq(0.001, SS_value * 1.3, length.out = i_SS),
#'                y = ( (1/(1 + parameter_grid[["n"]]) ) * (parameter_grid[["s"]] * parameter_grid[["B"]] * x^parameter_grid[["alpha"]] + (1 - parameter_grid[["delta"]]) * x) ) 
#'                )
#'   }else if(solow_variant == "GS"){
#'     
#'   }else if(solow_variant == "ESHC"){
#'     
#'   }else if(solow_variant == "ESSOE"){
#'     
#'   }else if(solow_variant == "ESSRO"){
#'     
#'   }else if(solow_variant == "ESSRL"){
#'     
#'   }else if(solow_variant == "ESSROL"){
#'     
#'   }
#'   aux_title <- paste("Transition Diagram of the", solow_variant)
#'   ggplot(aux_out) +
#'     geom_line(aes(x, y), size = 1.1) +
#'     geom_abline(intercept = 0, slope = 1, alpha = 0.8) +
#'     labs(title = aux_title, y = "KpW_{t + 1}", x = "KpW_{t}", caption = "Remark: The dashed line is the steady state.") +
#'     geom_vline(xintercept = SS_value, lty = 2, alpha = 0.5)
#' }
#' 
#' draw_solow_diagram <- function(sim_data, parameter_grid, solow_variant){
#'   # sim_data <- aux_simulation
#'   # parameter_grid <- aux_parameter_grid
#'   # solow_variant <- "BS"
#'   #' @title
#'   #' @description
#'   #' @inheritParams steadystate_checker
#'   
#'   theme_set(theme_bw())
#'   
#'   if(solow_variant == "BS"){
#'     SS_value <- BS_SS_KpW(parameter_grid[["B"]][i_SS], 
#'                           parameter_grid[["alpha"]][i_SS], 
#'                           parameter_grid[["s"]][i_SS], 
#'                           parameter_grid[["n"]][i_SS], 
#'                           parameter_grid[["delta"]][i_SS])
#'     i_SS <- (SS_value - sim_data[["KpW"]]) %>% between(0, SS_value * 0.001) %>% which() %>% min() # the period in which the SS is "reached" within in 0.1% 
#'     np <- dim(sim_data)[1]
#'     aux_out <- 
#'         tibble(x = seq(0.001, SS_value * 1.3, length.out = np),
#'                y1 = (1/(1+parameter_grid[["n"]]))*(parameter_grid[["s"]] * parameter_grid[["B"]] * x^parameter_grid[["alpha"]]),
#'                y2 = (1/(1+parameter_grid[["n"]]))*((parameter_grid[["n"]] + parameter_grid[["delta"]]) * x)
#'                )
#'   }else if(solow_variant == "GS"){
#'     
#'   }else if(solow_variant == "ESHC"){
#'     
#'   }else if(solow_variant == "ESSOE"){
#'     
#'   }else if(solow_variant == "ESSRO"){
#'     
#'   }else if(solow_variant == "ESSRL"){
#'     
#'   }else if(solow_variant == "ESSROL"){
#'     
#'   }
#'   aux_title <- paste("Transition Diagram of the", solow_variant)
#'   ggplot(aux_out) +
#'     geom_line(aes(x, y1)) +
#'     geom_line(aes(x, y2)) +
#'     labs(title = aux_title, y = "KpW_{t+1} - KpW_{t}", x = "KpW_{t}", caption = "Remark: The dashed line is the steady state.") +
#'     geom_vline(xintercept = SS_value, lty = 2, alpha = 0.5)
#' }
#' 
#' vector_shrinker <- function(vector, length_out){
#'   if(length(unique(vector)) == 1){
#'     rep(unique(vector), length_out)
#'   }else{
#'     unique_values <- unique(vector)
#'     value_percentages <- rep(NA, length(unique_values))
#'     for(i in seq_along(unique_values)){
#'       value_percentages[i] <- sum(vector == i)/length(vector)
#'     }
#'   }
#'   out_vector <- c()
#'   for(i in seq_along(unique_values)){
#'     out_vector <- c(out_vector, rep(unique_values[i], length_out * value_percentages[i]))
#'   }
#'   if(length(out_vector != length_out)){
#'     out_vector <- out_vector[1:length_out]
#'   }
#'   return(out_vector)
#' }
#' 
########################## end of section kept but not used ##########################
# 0.99 testing =================================
# vtstest <- c("testvar", "ja", "nein")
# create_simulation_table(vtstest, 20)
# variable_encoder(c("Rental Rate"))
# add_var_computer(tibble(L = 3, Y = 9, YpW = NA), c(T, T, F), c(), "exo", "BS")

# last_row_simulation <- testsimulation[nrow(testsimulation), ]
# last_row_parameter <- paragrid[nrow(paragrid), ]
# solow_variant <- "BS"
#
# result <- steadystate_checker(testsimulation[nrow(testsimulation), ],
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
