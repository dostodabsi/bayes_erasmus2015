library('rjags')
library('ggmcmc')
library('polspline')

# data from Zeelenberg et al. (2002), experiment 3
# adapted from Wagenmakers et al. (2010) on Savage-Dickey

# Wagenmakers et al. (2010) use a normal prior on effect size.
# following e.g. Rouder & Morey (2012) we use a cauchy prior
# the latter leads to a Bayes factor in favour of the alternative of 3.56 (normal prior = 4.42)

## note: when using the cauchy prior, the estimation is off and has high variance
# not sure why; also using polsplines apparently isn't the way to go (see Rouder & Morey, 2009)
## if you spot any errors (even embarrasingly obvious ones - I could miss those), email me!

n <- 74
N.SB = rep(21,n)
K.SB = c(15,11,15,14,15,18,16,16,18,16,15,13,18,12,11,13,17,18,16,11,17,18,12,18,18,14,21,
         18,17,10,11,12,16,18,17,15,19,12,21,15,16,20,15,19,16,16,14,18,16,19,17,11,19,18,
         16,16,11,19,18,12,15,18,20,8,12,19,16,16,16,12,18,17,11,20)

N.SN = rep(21,n)
K.SN = c(15,12,14,15,13,14,10,17,13,16,16,10,15,15,10,14,17,18,19,12,19,18,10,18,16,13,15,
         20,13,15,13,14,19,19,19,18,13,12,19,16,14,17,15,16,15,16,13,15,14,19,12,11,17,13,
         18,13,13,19,18,13,13,16,18,14,14,17,12,12,16,14,16,18,13,13)


model_string <- '
model {
    # group-level priors for study neither condition
    mu.phi ~ dnorm(0, 1)I(0,)
    sigma.phi ~ dunif(0, 10)
    tau.phi <- pow(sigma.phi, -2)

    # prior for group-level priming effect
    mu.alpha <- delta * sigma.alpha
    sigma.alpha ~ dunif(0, 10)
    tau.alpha <- pow(sigma.alpha, -2)

    # unit information prior on the effect size
    #delta ~ dnorm(0, 1)I(0,)

    # Cauchy in Rouder & Morey (2012)
    lambdadelta ~ dchisqr(1)
    delta ~ dnorm(0, lambdadelta)T(0, )

    for (i in 1:n) {
        K.SN[i] ~ dbin(theta.SN[i], N.SN[i])
        K.SB[i] ~ dbin(theta.SB[i], N.SB[i])

        theta.SN[i] <- phi(phi.SN[i])
        theta.SB[i] <- phi(phi.SB[i])

        # study condition comes from group-level prior
        phi.SN[i] ~ dnorm(mu.phi, tau.phi)

        # priming effect comes from a group-level prior
        alpha[i] ~ dnorm(mu.alpha, tau.alpha)

        # predict the response on a probit scale (additive)
        phi.SB[i] <- phi.SN[i] + alpha[i]
    }
}'

params <- c('delta')
data <- list('K.SN' = K.SN, 'K.SB' = K.SB, 'N.SN' = N.SN, 'N.SB' = N.SB, 'n' = n)

inits <- function() {
    # scale = 1 (better would be sqrt(2) / 2)
    list(phi.SN = rnorm(n), alpha = rnorm(n), mu.phi = abs(rnorm(1)),
         lambdadelta = rchisq(1, 1), sigma.phi = runif(1, 0, 5),
         sigma.alpha = runif(1, 0, 5), delta = abs(rcauchy(1)))
}

model <- jags.model(textConnection(model_string), n.adapt = 500, inits = inits,
                    data = data, n.chains = 3)
samples <- coda.samples(model, n.iter = 50000, variable.names = params, n.thin = 1)


# estimate BF using Savage-Dickey
savage_dickey <- function(samples, prior = 2 * dcauchy(0)) {
    fit.posterior <- logspline(samples)
    posterior <- dlogspline(0, fit.posterior)
    BF01 <- posterior / prior
    BF01
}

# uncomment for info / plots
#summary(samples) 
#ggs_density(ggs(samples))
#ggs_traceplot(ggs(samples))
