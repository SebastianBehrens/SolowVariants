# Model Functions of Extended Solow Growth Model with Endogeneous Growth ---------------------------------
ESEG_MF_KN <- function(s, Y, delta, K){s * Y + (1-delta)*K}
ESEG_MF_LN <- function(n, L){(1+n) * L}
# ESEG_MF_RR <- function(K, L, alpha){alpha * B * (K/L)^(alpha - 1)}
# ESEG_MF_WR <- function(K, L, alpha){(1-alpha) * B * (K/L)^alpha}
ESEG_MF_Y <- function(K, L, alpha, phi){K^alpha * (K^phi*L)^(1-alpha)}

# ESEG_SS_gY <- function(endogenous_type, input_list){
#     if(endogenous_type == "AK")
#     (1 + n)^((phi)/(1- phi)) - 1
# }

ESEG_SS_gYpW <- function(n, phi, s, A, delta){
    if(phi < 1){
    (1 + n)^((phi)/(1- phi)) - 1
    }else if(i %>% between(0.95, 1)){
            s * A - delta
    }else if(phi > 1){
            NaN
    }
}
ESEG_SS_KpEW <- function(alpha, phi, n, s, delta){(s/((1 + n)^(1/(1 - phi)) - (1- delta)))^(1/(1-alpha))}
# BS_SS_KpW <- function(B, alpha, s, n, delta){B^(1/(1-alpha))*(s/(n+ delta))^(1/(1-alpha))}
# BS_SS_YpW <- function(B, alpha, s, n, delta){B^(1/(1-alpha))*(s/(n+ delta))^(alpha/(1-alpha))}
# BS_SS_CpW <- function(B, alpha, s, n, delta){B^(1/(1-alpha))*(1-s)*(s/(n+delta))^(alpha/(1-alpha))}
# BS_SS_RR <- function(alpha, s, n, delta){alpha * (s/(n + delta))^(-1)}
# BS_SS_WR <- function(B, alpha, s, n, delta){(1-alpha)*B^(1/(1-alpha))*(s/(n+delta))^(alpha/(1-alpha))}