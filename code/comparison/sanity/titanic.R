library('rjags')
library('ggmcmc')
library('polspline')

# logistic regression on titanic survivor data
# comparison of Savage-Dickey Bayes factor and BIC approximation (not hierarchical!)

dat <- read.csv('titanic3.csv')
dat <- dat[!is.na(dat$age), ] # don't want to model NA ...

set.seed(1774) # Laplace easter egg ;)
dat$test <- runif(nrow(dat))


model_string <- '
model {
    # prior on effect size (cf. Rouder & Morey, 2012)
    delta ~ dnorm(0, lambdadelta)

    lambdadelta ~ dchisqr(1)
    sigma ~ dunif(0, 10)

    b0 ~ dnorm(0, 0.001)
    b1 ~ dnorm(0, 0.001)
    b2 <- delta * sigma

    for (i in 1:N) {
        y[i] ~ dbern(p[i])
        logit(p[i]) <- b0 + b1 * x1[i] + b2 * x2[i]
    }
}'

params <- c('delta')
data <- list('y' = dat$survived, 'x1' = dat$age, 'x2' = dat$test, 'N' = nrow(dat))

model <- jags.model(textConnection(model_string), data = data, n.adapt = 1000, n.chains = 3)
samples <- coda.samples(model, n.iter = 40000, variable.names = params)

savage_dickey <- function(samples, prior = dcauchy(0)) {
    fit.posterior <- logspline(samples)
    posterior <- dlogspline(0, fit.posterior)
    BF01 <- posterior / prior
    BF01
}

BF01_bic <- function(h1, h0) exp((BIC(h1) - BIC(h0)) / 2)


# frequentist models
freq_age <- glm(survived ~ age, dat, family = binomial)
freq_age_test <- glm(survived ~ age + test, dat, family = binomial)

# BIC approximation
BF_bic <- BF01_bic(freq_age_test, freq_age)

# Bayesian testing: Savage-Dickey
BF_savage <- savage_dickey(samples)
