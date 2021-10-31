# Model Functions of Extended Solow Growth Model with the Scarce Resource Oil ---------------------------------
ESSRO_MF_Y <- function(A, K, L, E, alpha, beta){K^alpha * (A*L)^(beta) * E^(1- alpha - beta)}
ESSRO_MF_KN <- function(s, Y, delta, K){s * Y + (1-delta)*K}
ESSRO_MF_LN <- function(n, L){(1+n) * L}
ESSRO_MF_RN <- function(E, R){R - E}
ESSRO_MF_E <- function(sE, R){sE * R}
ESSRO_MF_AN <- function(g, A){(1+g) * A}

# ESSRO_MF_RR <- function(){}
# ESSRO_MF_WR <- function(){}

ESSRO_SS_YpW <- function(KpW, YpW, A, R, L, alpha, beta){
    epsilon <- 1- alpha - beta
    (KpW/YpW)^(alpha/(beta + epsilon)) * A^(beta/(beta + epsilon)) * (R/L)^(epsilon/(beta + epsilon))}


# Remark regarding dynamics of E and R
# R_0 => E_1 = s_ER_0 => R_1=R_0 - E_1 => E_2=sER_1 => ...