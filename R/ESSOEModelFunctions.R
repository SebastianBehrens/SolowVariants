# Model Functions of Extended Solow Growth Model for the Small and Open Economy ---------------------------------
ESSOE_MF_LN <- function(n, L){(1+n) * L}
ESSOE_MF_RR <- function(B, K, L, alpha){alpha * B * (K/(L))^(alpha - 1)}
ESSOE_MF_WR <- function(B, K, L, alpha){B* (1-alpha) * (K/(L))^alpha}
ESSOE_MF_K <- function(r, alpha, B, L){L/((r/(alpha * B))^(1/(1-alpha)))}
ESSOE_MF_Y <- function(B, K, L, alpha){B* K^alpha * (L)^(1-alpha)}
ESSOE_MF_Yn <- function(Y, r, F_var){Y + r * F_var} # F_var for variable F since F in R stands for FALSE
ESSOE_MF_VN <- function(Y_nat, s, V_previous){s*Y_nat + V_previous}
ESSOE_MF_F <- function(V, K){V - K}


ESSOE_SS_KpW <- function(B, alpha, r){B^(1/(1-alpha))*(alpha/r)^(1/(1-alpha))}
ESSOE_SS_YpW <- function(B, alpha, r){B^(1/(1-alpha))*(alpha/r)^(alpha/(1-alpha))}
ESSOE_SS_WR <- function(B, alpha, r){(1-alpha) * B^(1/(1-alpha))*(alpha/r)^(alpha/(1-alpha))}
ESSOE_SS_VpW <- function(s, n, r, w){((s/n)/(1 - (s/n)*r))*w} # old
ESSOE_SS_VpW <- function(s, n, r, w){(s/(n- s * r)) * w}
ESSOE_SS_FpW <- function(alpha, s, n, r, w){(1/(1- alpha))* (s/n) * (1/r)*((r - ((alpha* n)/s))/(1 - (s/n)*r))*w}
