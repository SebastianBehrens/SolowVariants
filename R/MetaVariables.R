


# Setting up all variables (standardize down below) =================================
# this messy and redundant way was created first and is standardized down below

# meta_BS_variables <-
#     c(
#         "Output",
#         "Consumption",
#         "Capital Stock",
#         "Labor Stock",
#         "Output per Worker",
#         "Consumption per Worker",
#         "Capital Stock per Worker",
#         "Output per Effective Worker",
#         "Consumption per Effective Worker",
#         "Capital Stock per Effective Worker",

#         "Wage Rate",
#         "Rental Rate",
#         "Capital to Output Ratio",

#         "Log of Capital Stock",
#         "Log of Capital Stock per Worker",
#         "Log of Capital Stock per Effective Worker",

#         "Log of Output",
#         "Log of Output per Worker",
#         "Log of Output per Effective Worker",

#         "Growth Rate of Output",
#         "Growth Rate of Output per Worker",
#         "Growth Rate of Output per Effective Worker",

#         "Growth Rate of Capital Stock",
#         "Growth Rate of Capital Stock per Worker",
#         "Growth Rate of Capital Stock per Effective Worker"
#     )

# meta_GS_variables <-
#     c(
#         "Capital Stock",
#         "Labor Stock",
#         "Output",
#         "Consumption",
        
#         "Capital Stock per Worker",
#         "Output per Worker",
#         "Consumption per Worker",
        
#         "Capital Stock per Effective Worker",
#         "Output per Effective Worker",
#         "Consumption per Effective Worker",
        
#         "Wage Rate",
#         "Rental Rate",
#         "Capital to Output Ratio",
        
#         "Log of Output",
#         "Log of Output per Worker",
#         "Log of Output per Effective Worker",
        
#         "Log of Capital Stock",
#         "Log of Capital Stock per Worker",
#         "Log of Capital Stock per Effective Worker",
        
#         "Growth Rate of Output",
#         "Growth Rate of Output per Worker",
#         "Growth Rate of Output per Effective Worker",
        
#         "Growth Rate of Capital Stock",
#         "Growth Rate of Capital Stock per Worker",
#         "Growth Rate of Capital Stock per Effective Worker",
        
#         "Total Factor Productivity"
#     )

# meta_ESSOE_variables <-
#     c(
#         "Capital Stock",
#         "Labor Stock",
#         "Output",
        
#         "Capital Stock per Worker",
#         "Output per Worker",
        
#         "Capital Stock per Effective Worker",
#         "Output per Effective Worker",
        
#         "Wage Rate",
#         "Rental Rate",
#         "Capital to Output Ratio",
        
#         "National Output",
#         "National Wealth",
#         "National Wealth per Worker",
#         "National Wealth per Effective Worker",
#         "Net Foreign Assets",
#         "Net Foreign Assets per Worker",
#         "National Savings",
        
#         "Log of Output",
#         "Log of Output per Worker",
#         "Log of Output per Effective Worker",
        
#         "Log of Capital Stock",
#         "Log of Capital Stock per Worker",
#         "Log of Capital Stock per Effective Worker",
        
#         "Growth Rate of Output",
#         "Growth Rate of Output per Worker",
#         "Growth Rate of Output per Effective Worker",
        
#         "Growth Rate of Capital Stock",
#         "Growth Rate of Capital Stock per Worker",
#         "Growth Rate of Capital Stock per Effective Worker"
        
#     )
# meta_ESHC_variables <-
#     c(
#         "Total Factor Productivity",
#         "Human Capital Stock",
#         "Physical Capital Stock",
#         "Labor Stock",
#         "Output",
        
#         "Human Capital Stock per Worker",
#         "Capital Stock per Worker",
#         "Output per Worker",
        
#         "Human Capital Stock per Effective Worker",
#         "Capital Stock per Effective Worker",
#         "Output per Effective Worker",
        
#         "Wage Rate",
#         "Rental Rate",
#         "Physical Capital to Output Ratio",
#         "Human Capital to Output Ratio",
        
#         "Log of Output",
#         "Log of Output per Worker",
#         "Log of Output per Effective Worker",
        
#         "Log of Capital Stock",
#         "Log of Capital Stock per Worker",
#         "Log of Capital Stock per Effective Worker",
        
#         "Log of Human Capital Stock",
#         "Log of Human Capital Stock per Worker",
#         "Log of Human Capital Stock per Effective Worker",
        
#         "Growth Rate of Output",
#         "Growth Rate of Output per Worker",
#         "Growth Rate of Output per Effective Worker",
        
#         "Growth Rate of Capital Stock",
#         "Growth Rate of Capital Stock per Worker",
#         "Growth Rate of Capital Stock per Effective Worker",
        
#         "Growth Rate of Human Capital Stock",
#         "Growth Rate of Human Capital Stock per Worker",
#         "Growth Rate of Human Capital Stock per Effective Worker"
#     )

# meta_ESSRO_variables <-
#     c(
#         "Capital Stock",
#         "Labor Stock",
#         "Energy Use",
#         "Resource Stock",
#         "Output",
        
#         "Capital Stock per Worker",
#         "Output per Worker",
        
#         "Capital Stock per Effective Worker",
#         "Output per Effective Worker",
        
#         "Wage Rate",
#         "Rental Rate",
        
#         "Log of Output",
#         "Log of Output per Worker",
#         "Log of Output per Effective Worker",
        
#         "Log of Capital Stock",
#         "Log of Capital Stock per Worker",
#         "Log of Capital Stock per Effective Worker",
        
#         "Growth Rate of Output",
#         "Growth Rate of Output per Worker",
#         "Growth Rate of Output per Effective Worker",
        
#         "Growth Rate of Capital Stock",
#         "Growth Rate of Capital Stock per Worker",
#         "Growth Rate of Capital Stock per Effective Worker",
        
#         "Total Factor Productivity"
#     )

# meta_ESSRL_variables <-
#     c(
#         "Total Factor Productivity",
#         "Capital Stock",
#         "Labor Stock",
#         "Output",
        
#         "Capital Stock per Worker",
#         "Output per Worker",
        
#         "Capital Stock per Effective Worker",
#         "Output per Effective Worker",
        
#         "Wage Rate",
#         "Capital Rental Rate",
#         "Land Rental Rate",
#         "Capital to Output Ratio",
        
#         "Log of Output",
#         "Log of Output per Worker",
#         "Log of Output per Effective Worker",
        
#         "Log of Capital Stock",
#         "Log of Capital Stock per Worker",
#         "Log of Capital Stock per Effective Worker",
        
#         "Growth Rate of Output",
#         "Growth Rate of Output per Worker",
#         "Growth Rate of Output per Effective Worker",
        
#         "Growth Rate of Capital Stock",
#         "Growth Rate of Capital Stock per Worker",
#         "Growth Rate of Capital Stock per Effective Worker"
#     )
# meta_ESSROL_variables <-
#     c(
#         "Capital Stock",
#         "Labor Stock",
#         "Energy Use",
#         "Resource Stock",
#         "Output",
        
#         "Capital Stock per Worker",
#         "Output per Worker",
        
#         "Capital Stock per Effective Worker",
#         "Output per Effective Worker",
        
#         "Wage Rate",
#         "Rental Rate",
#         "Capital to Output Ratio",
        
#         "Log of Output",
#         "Log of Output per Worker",
#         "Log of Output per Effective Worker",
        
#         "Log of Capital Stock",
#         "Log of Capital Stock per Worker",
#         "Log of Capital Stock per Effective Worker",
        
#         "Growth Rate of Output",
#         "Growth Rate of Output per Worker",
#         "Growth Rate of Output per Effective Worker",
        
#         "Growth Rate of Capital Stock",
#         "Growth Rate of Capital Stock per Worker",
#         "Growth Rate of Capital Stock per Effective Worker",
        
#         "Total Factor Productivity"
#     )

# meta_ESEG_variables <-
#     c(
#         "Output",
#         "Consumption",
#         "Capital Stock",
#         "Labor Stock",
#         "Total Factor Productivity",
#         "Output per Worker",
#         "Consumption per Worker",
#         "Capital Stock per Worker",
#         "Output per Effective Worker",
#         "Consumption per Effective Worker",
#         "Capital Stock per Effective Worker",
        
#         "Wage Rate",
#         "Rental Rate",
#         "Capital to Output Ratio",
        
#         "Log of Capital Stock",
#         "Log of Capital Stock per Worker",
#         "Log of Capital Stock per Effective Worker",
        
#         "Log of Output",
#         "Log of Output per Worker",
#         "Log of Output per Effective Worker",
        
#         "Growth Rate of Output",
#         "Growth Rate of Output per Worker",
#         "Growth Rate of Output per Effective Worker",
        
#         "Growth Rate of Capital Stock",
#         "Growth Rate of Capital Stock per Worker",
#         "Growth Rate of Capital Stock per Effective Worker"
#     )

# meta_ESEGRomer_variables <-
#     c(
#         "Output",
#         "Consumption",
#         "Capital Stock",
#         "Labor Stock",
#         "Total Factor Productivity",
#         "Output per Worker",
#         "Consumption per Worker",
#         "Capital Stock per Worker",
#         "Output per Effective Worker",
#         "Consumption per Effective Worker",
#         "Capital Stock per Effective Worker",
        
#         "Wage Rate",
#         "Rental Rate",
#         "Capital to Output Ratio",
        
#         "Log of Capital Stock",
#         "Log of Capital Stock per Worker",
#         "Log of Capital Stock per Effective Worker",
        
#         "Log of Output",
#         "Log of Output per Worker",
#         "Log of Output per Effective Worker",
        
#         "Growth Rate of Output",
#         "Growth Rate of Output per Worker",
#         "Growth Rate of Output per Effective Worker",
        
#         "Growth Rate of Capital Stock",
#         "Growth Rate of Capital Stock per Worker",
#         "Growth Rate of Capital Stock per Effective Worker"
#     )

# save(meta_BS_variables, meta_GS_variables, meta_ESSOE_variables, meta_ESHC_variables, meta_ESSRO_variables, meta_ESSRL_variables, meta_ESSROL_variables,meta_ESEG_variables , meta_ESEGRomer_variables, file = "meta_variables.Rdata")

# Simplifying and Standardizing the variables =================================
# meta_BS_variables
# meta_GS_variables
# meta_ESHC_variables
# meta_ESSOE_variables
# meta_ESSRO_variables
# meta_ESSRL_variables
# meta_ESSROL_variables
# meta_ESEG_variables
# meta_ESEGRomer_variables

# base_variables <- intersect(meta_BS_variables,
#                             meta_GS_variables)
# variables <- list(base = base_variables)
# eval_string <- function(string){eval(parse(text = string))}
# for(j in c("BS", "GS", "ESSOE", "ESHC", "ESSRO", "ESSRL", "ESSROL", "ESEG", "ESEGRomer")){
#     var_string_j <- paste0("meta_", j, "_variables")
#     share_code_string <- paste0(var_string_j, " %in% variables$base")
#     indices_shared <- eval_string(share_code_string)
#     variables[[j]] <- eval_string(paste0(var_string_j, "[!indices_shared]"))
# }
# modelcodes <- c("BS", "GS", "ESSOE", "ESHC", "ESSRO", "ESSRL", "ESSROL", "ESEG", "ESEGRomer")
# for(i in c(1:9)){
#     assign(paste0("add_vars_", modelcodes[i]), variables[[i+1]])
# }

# add_vars_BS
# add_vars_GS
# add_vars_ESSOE
# add_vars_ESHC
# add_vars_ESSRO
# add_vars_ESSRL
# add_vars_ESSROL
# add_vars_ESEG
# add_vars_ESEGRomer

# save(base_variables, add_vars_BS, add_vars_GS, add_vars_ESSOE, add_vars_ESHC, add_vars_ESSRO, add_vars_ESSRL, add_vars_ESSROL, add_vars_ESEG, add_vars_ESEGRomer, file = "meta_variables.Rdata")

