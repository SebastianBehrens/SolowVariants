# Model Functions of General Solow Growth Model ---------------------------------
GS_MF_KN <- function(s, Y, delta, K){s * Y + (1-delta)*K}
GS_MF_LN <- function(n, L){(1+n) * L}
GS_MF_AN <- function(g, A){(1+g) * A}
GS_MF_RR <- function(A, K, L, alpha){alpha * (K/(A*L))^(alpha - 1)}
GS_MF_WR <- function(A, K, L, alpha){A* (1-alpha) * (K/(A*L))^alpha }
GS_MF_Y <- function(A, K, L, alpha){K^alpha * (A*L)^(1-alpha)}

# from book
GS_SS_KpW <- function(s, n, g, delta, alpha, A){A * (s/(n + g + delta + n*g))^(1/(1-alpha))}
GS_SS_YpW <- function(s, n, g, delta, alpha, A){A * (s/(n + g + delta + n*g))^(alpha/(1-alpha))}
GS_SS_CpW <- function(s, n, g, delta, alpha, A){A * (1-s) * (s/(n + g + delta + n * g))^(alpha/(1-alpha))}
GS_SS_RR <- function(alpha, s, n, g, delta){alpha * (s/(n + g + delta + n * g))^(-1)}
GS_SS_WR <- function(alpha, s, n, g, delta, A){A * (1- alpha) * (s/(n + g + delta + n * g))^(alpha/(1-alpha))}

# from slides
GS_SS_KpEW <- function(s, n, g, delta, alpha){(s/(n + g + delta + n*g))^(1/(1-alpha))}
GS_SS_YpEW <- function(s, n, g, delta, alpha, A){(s/(n + g + delta + n*g))^(alpha/(1-alpha))}
