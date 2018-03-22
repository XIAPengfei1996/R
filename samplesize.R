## Individually Matched Studies with 1:r Matching ##

samplesize <- function(P0, OR, alpha = 0.05, beta = 0.10, r = 1) {
    za <- qnorm(1 - alpha / 2)
    zb <- qnorm(1 - beta)
    P <- OR / (1 + OR)
    m <- (1 + r) * (za / 2 + zb * sqrt(P * (1 - P))) ^ 2 / (2 * r * (P - 0.5) ^ 2)
    P1 <- (OR * P0) / (1 - P0 + OR * P0)
    cases <- m / (P0 * (1 - P1) + P1 * (1 - P0))
    controls <- cases * r
}

## Parker R A, Bregman D J. Sample size for individually matched case-control studies.[J]. Biometrics, 1986, 42(4):919-926