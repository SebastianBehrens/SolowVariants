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
