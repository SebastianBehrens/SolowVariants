# Model Functions of Extended Solow Growth Model with the Scarce Resources Oil and Land ---------------------------------
ESSROL_MF_Y <- function(A, K, L, E, X, alpha, beta, kappa){K^alpha * (A*L)^(beta) * X^kappa * E^(1-alpha-beta-kappa)}
ESSROL_MF_KN <- function(s, Y, delta, K){s * Y + (1-delta)*K}
ESSROL_MF_LN <- function(n, L){(1+n) * L}
ESSROL_MF_RN <- function(E, R){R - E}
ESSROL_MF_E <- function(sE, R){sE * R}
ESSROL_MF_AN <- function(g, A){(1+g) * A}
# ESSRO_MF_RR <- function(){}
# ESSRO_MF_WR <- function(){}

ESSROL_SS_gY <- function(alpha, beta, kappa, n, g, sE){
    (beta/(beta + kappa + (1- alpha - beta - kappa)))* g - 
        ((kappa + (1- alpha - beta- kappa))/(beta + kappa + (1- alpha - beta - kappa))) * n - 
        ((1- alpha - beta- kappa)/(beta + kappa  + (1- alpha - beta - kappa))) * sE
}

# Remark regarding dynamics of E and R
# R_0 => E_1 = s_ER_0 => R_1=R_0 - E_1 => E_2=sER_1 => ...