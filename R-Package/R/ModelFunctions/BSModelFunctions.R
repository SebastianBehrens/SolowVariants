# Model Functions of Basic Solow Model ---------------------------------
BS_MF_KN <- function(s, Y, delta, K){s * Y + (1-delta)*K}
BS_MF_LN <- function(n, L){(1+n) * L}
BS_MF_RR <- function(B, K, L, alpha){alpha * B * (K/L)^(alpha - 1)}
BS_MF_WR <- function(B, K, L, alpha){(1-alpha) * B * (K/L)^alpha}
BS_MF_Y <- function(B, K, L, alpha){B * K^alpha * L^(1-alpha)}

BS_SS_KpW <- function(B, alpha, s, n, delta){B^(1/(1-alpha))*(s/(n+ delta))^(1/(1-alpha))}
BS_SS_YpW <- function(B, alpha, s, n, delta){B^(1/(1-alpha))*(s/(n+ delta))^(alpha/(1-alpha))}
BS_SS_CpW <- function(B, alpha, s, n, delta){B^(1/(1-alpha))*(1-s)*(s/(n+delta))^(alpha/(1-alpha))}
BS_SS_RR <- function(alpha, s, n, delta){alpha * (s/(n + delta))^(-1)}
BS_SS_WR <- function(B, alpha, s, n, delta){(1-alpha)*B^(1/(1-alpha))*(s/(n+delta))^(alpha/(1-alpha))}