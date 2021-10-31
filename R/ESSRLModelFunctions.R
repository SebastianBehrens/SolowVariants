# Model Functions of Extended Solow Growth Model with the Scarce Resource Land ---------------------------------
ESSRL_MF_Y <- function(A, K, L, X, alpha, beta){K^alpha * (A * L)^(beta) * X^(1-alpha - beta)}

ESSRL_MF_KN <- function(s, Y, delta, K){s * Y + (1-delta)*K}
ESSRL_MF_LN <- function(n, L){(1+n) * L}
ESSRL_MF_AN <- function(g, A){(1+g) * A}

ESSRL_MF_WR <- function(A, K, L, X, alpha, beta){beta * (K/(A*L))^alpha * (X/(A*L))^(1-alpha - beta) * A}
ESSRL_MF_RR <- function(A, K, L, X, alpha, beta){alpha * (K/(A*L))^(alpha - 1) * (X/(A*L))^(1-alpha - beta)}
ESSRL_MF_LR <- function(A, K, L, X, alpha, beta){(1-alpha - beta) * (K/(A*L))^(alpha) * (X/(A*L))^((1-alpha - beta) - 1)}

# from book (but also on slides)
# Capital to Output Ratio
ESSRL_SS_YpW <- function(KpW, YpW, A, X, L, alpha, beta){
    (KpW/YpW)^(alpha/(beta + (1-alpha - beta))) * A^(beta/(beta + (1-alpha - beta))) * (X/L)^((1-alpha - beta)/(beta + (1-alpha - beta)))}
ESSRL_SS_CtO <- function(s, n, g, delta, alpha, beta){
    s/(((1 + n) * (1 + g))^(beta/(beta + (1-alpha - beta))) - (1- delta))}


# Remember: (1-alpha - beta) => this is kappa.
