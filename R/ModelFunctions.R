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

# Model Functions of Extended Solow Growth Model with Human Capital ---------------------------------
ESHC_MF_KN <- function(sK, Y, delta, K){sK * Y + (1-delta)*K}
ESHC_MF_HN <- function(sH, Y, delta, H){sH * Y + (1-delta)*H}
ESHC_MF_LN <- function(n, L){(1+n) * L}
ESHC_MF_AN <- function(g, A){(1+g) * A}
ESHC_MF_RR <- function(A, H, K, L, alpha, phi){alpha * (K/(A*L))^(alpha - 1)*(H/(A*L))^phi}
ESHC_MF_WR <- function(A, H, K, L, alpha, phi){(1-alpha) * (K/(A*L))^(alpha)*(H/(A*L))^phi * A}
ESHC_MF_Y <- function(A, H, K, L, alpha, phi){K^alpha * H^phi * (A*L)^(1-alpha - phi)}
ESHC_MF_KpEWN <- function(n, g, sK, YpEW, delta, KpEW){(1/((1+n)*(1+g))) * (sK * YpEW + (1-delta)*KpEW)}
ESHC_MF_HpEWN <- function(n, g, sH, YpEW, delta, KpEW){(1/((1+n)*(1+g))) * (sH * YpEW + (1-delta)*KpEW)}
# from book
ESHC_SS_KpEW <- function(sK, sH, alpha, phi, n, g, delta){((sK^(1-phi)*sH^phi)/(n + g + delta + n * g))^(1/(1-alpha - phi))}
ESHC_SS_HpEW <- function(sK, sH, alpha, phi, n, g, delta){((sK^alpha * sH^(1-alpha))/(n + g + delta + n * g))^(1/(1-alpha- phi))} 

# YpEW complicated variant (sH and sK occuring twice per factor)
# ESHC_SS_YpEW <- function(sK, sH, n, g, delta, alpha, phi){(((sK^(1-phi)*sH^phi)/(n + g + delta + n * g))^(1/(1-alpha - phi)))^alpha *(((sK^(1- alpha) * sH^alpha)/(n + g + delta + n * g))^(1/(1-alpha- phi)))^phi}

# YpEW simplified sK and sH occuring just once in separate factors
ESHC_SS_YpEW <- function(sK, sH, n, g, delta, alpha, phi){(((sK)/(n + g + delta + n * g))^(1/(1-alpha - phi)))^alpha *(((sH)/(n + g + delta + n * g))^(1/(1-alpha- phi)))^phi}

ESHC_SS_YpW <- function(A, sK, sH, n, g, delta, alpha, phi){A * (((sK)/(n + g + delta + n * g))^(1/(1-alpha - phi)))^alpha *(((sH)/(n + g + delta + n * g))^(1/(1-alpha- phi)))^phi}
# ESHC_SS_YpW <- function(HpEW, KpEW, A, alpha, phi){A * KpEW^alpha * HpEW^phi}

ESHC_SS_CpW <- function(A, sK, sH, n, g, delta, alpha, phi){A * (1- sK - sH) * (((sK)/(n + g + delta + n * g))^(1/(1-alpha - phi)))^alpha *(((sH)/(n + g + delta + n * g))^(1/(1-alpha- phi)))^phi}

# SS_WR and SS_RR missing. formula nowhere to be found — must be computed myself
# ESHC_SS_RR <- function(alpha, s, n, g, delta){alpha * (s/(n + g + delta + n * g))^(-1)}
# ESHC_SS_WR <- function(alpha, s, n, g, delta, A){A * (1- alpha) * (s/(n + g + delta + n * g))^(alpha/(1-alpha))}


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

# Model Functions of Extended Solow Growth Model with the Scarce Resource Land ---------------------------------
ESSRL_MF_Y <- function(A, K, L, X, alpha, beta){K^alpha * (A * L)^(beta) * X^(1-alpha - beta)}

ESSRL_MF_KN <- function(s, Y, delta, K){s * Y + (1-delta)*K}
ESSRL_MF_LN <- function(n, L){(1+n) * L}
ESSRL_MF_AN <- function(g, A){(1+g) * A}

ESSRL_MF_WR <- function(A, K, L, X, alpha, beta){beta * (K/(A*L))^alpha * (X/(A*L))^(1-alpha - beta) * A}
ESSRL_MF_RR <- function(A, K, L, X, alpha, beta){alpha * (K/(A*L))^(alpha - 1) * (X/(A*L))^(1-alpha - beta)}
ESSRL_MF_LR <- function(A, K, L, X, alpha, beta){(1-alpha - beta) * (K/(A*L))^(alpha) * (X/(A*L))^((1-alpha - beta) - 1)}

# from book (but also on slides)
ESSRL_SS_YpW <- function(KpW, YpW, A, X, L, alpha, beta){
    (KpW/YpW)^(alpha/(beta + (1-alpha - beta))) * A^(beta/(beta + (1-alpha - beta))) * (X/L)^((1-alpha - beta)/(beta + (1-alpha - beta)))}
# Capital to Output Ratio
ESSRL_SS_CtO <- function(s, n, g, delta, alpha, beta){
    s/(((1 + n) * (1 + g))^(beta/(beta + (1-alpha - beta))) - (1- delta))}


# Remember: (1-alpha - beta) => this is kappa.

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


# Model Functions of Extended Solow Growth Model with Endogeneous Growth as developed by Romer ---------------------------------
ESEGRomer_MF_KN <- function(s, Y, delta, K){s * Y + (1-delta)*K}
ESEGRomer_MF_LN <- function(n, L){(1+n) * L}
ESEGRomer_MF_L_At <- function(sR, L)
ESEGRomer_MF_AN <- function(rho, phi, lambda, A, L_At){rho * A^phi * L_At^lambda + A}
# ESEG_MF_RR <- function(K, L, alpha){alpha * B * (K/L)^(alpha - 1)}
# ESEG_MF_WR <- function(K, L, alpha){(1-alpha) * B * (K/L)^alpha}
ESEGRomer_MF_Y <- function(K, L, alpha, phi){K^alpha * (K^phi*L)^(1-alpha)}

# ESEG_SS_gY <- function(endogenous_type, input_list){
#     if(endogenous_type == "AK")
#     (1 + n)^((phi)/(1- phi)) - 1
# }

ESEGRomer_SS_gYpW <- function(n, phi, s, A, delta){
    if(phi < 1){
    (1 + n)^((phi)/(1- phi)) - 1
    }else if(i %>% between(0.95, 1)){
            s * A - delta
    }else if(phi > 1){
            NaN
    }
}
ESEGRomer_SS_KpEW <- function(alpha, phi, n, s, delta){(s/((1 + n)^(1/(1 - phi)) - (1- delta)))^(1/(1-alpha))}
# BS_SS_KpW <- function(B, alpha, s, n, delta){B^(1/(1-alpha))*(s/(n+ delta))^(1/(1-alpha))}
# BS_SS_YpW <- function(B, alpha, s, n, delta){B^(1/(1-alpha))*(s/(n+ delta))^(alpha/(1-alpha))}
# BS_SS_CpW <- function(B, alpha, s, n, delta){B^(1/(1-alpha))*(1-s)*(s/(n+delta))^(alpha/(1-alpha))}
# BS_SS_RR <- function(alpha, s, n, delta){alpha * (s/(n + delta))^(-1)}
# BS_SS_WR <- function(B, alpha, s, n, delta){(1-alpha)*B^(1/(1-alpha))*(s/(n+delta))^(alpha/(1-alpha))}