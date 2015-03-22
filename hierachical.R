library('lme4')
library('rjags')
library('ggmcmc')


dat <- read.table('hsbdataset.txt', header = TRUE)
dat <- dat[, c('mathach', 'school', 'cses')]
dat$school <- as.factor(as.numeric(as.factor(dat$school)))

N <- nrow(dat)
n.schools <- length(unique(dat$school))


model_string <- '
model {
    ## hyperpriors

    # -- random intercept
    mu.slope ~ dnorm(0, 0.001)
    sigma.slope ~ dunif(0, 100)
    tau.slope <- pow(sigma.slope, -2)

    # -- random slope
    mu.int ~ dnorm(0, 0.001)
    sigma.int ~ dunif(0, 100)
    tau.int <- pow(sigma.int, -2)

    # first-level priors
    sigma ~ dunif(0, 100)
    tau <- pow(sigma, -2)

    for (i in 1:n.schools) {
        b0[i] ~ dnorm(mu.int, tau.int) # intercept
        b1[i] ~ dnorm(mu.slope, tau.slope) # slope
    }

    for (i in 1:N) {
        # get intercept & slope of the group
        mu[i] <- b0[school[i]] + b1[school[i]] * x[i]
        y[i] ~ dnorm(mu[i], tau)
    }
}'

data <- list('x' = dat$cses, 'y' = dat$mathach, 'N' = N,
            'school' = dat$school, 'n.schools' = n.schools)

myinits <- function() {
    inits <- list('b0' = rnorm(n.schools), 'b1' = rnorm(n.schools),
                  'mu.int' = rnorm(1), 'sigma.int' = rlnorm(1),'sigma' = rlnorm(1),
                  'mu.slope' = rnorm(1), 'sigma.slope' = rlnorm(1))
    inits
}

params <- c('b0', 'b1', 'mu.int', 'sigma.int', 'mu.slope', 'sigma.slope')
model <- jags.model(textConnection(model_string), n.adapt = 100,
                    data = data, inits = myinits, quiet = TRUE)
samples <- coda.samples(model, n.iter = 1000, variable.names = params)
gdat <- ggs(samples)
print(summary(samples))

# Frequentist Approach
freq <- lmer(mathach ~ cses + (cses | school), data = dat, REML = FALSE)
print(summary(freq))
