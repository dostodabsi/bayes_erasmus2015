dat <- read.table('hsbdataset.txt', header = TRUE)
dat <- dat[, c('mathach', 'school', 'cses', 'sector')]
dat$school <- as.factor(as.numeric(as.factor(dat$school)))

N <- nrow(dat)
n.schools <- length(unique(dat$school))


data <- list('x' = dat$cses, 'y' = dat$mathach, 'N' = N,
            'school' = dat$school, 'n.schools' = n.schools)

data_sector <- list('x1' = dat$cses, 'x2' = dat$sector, 'y' = dat$mathach, 'N' = N,
             'school' = dat$school, 'n.schools' = n.schools)


# functions to estimate the model and draw from the posterior
get_model <- function(file, inits, data, dir = 'models', ...) {
    file <- paste(dir, file, sep = '/')
    jags.model(file = file, n.adapt = 1000, n.chains = 2, data = data, ...)
}

get_samples <- function(model, params, ...) {
    coda.samples(model, variable.names = params, n.iter = 10000, ...)
}


# initial values for MC for random effects models
inits <- list(
     'fixed' = function() {
         list('b0' = rnorm(1), 'b1' = rnorm(n.schools), 'sigma' = rlnorm(1))
     },
     'rint' = function() {
         list('b0' = rnorm(n.schools), 'b1' = rnorm(1), 'sigma' = rlnorm(1),
              'mu.int' = rnorm(1), 'sigma.int' = rlnorm(1))
     },
     'rslope' = function() {
         list('b0' = rnorm(1), 'b1' = rnorm(n.schools), 'sigma' = rlnorm(1),
              'mu.slope' = rnorm(1), 'sigma.slope' = rlnorm(1))
     },
    'maximal' = function() {
         list('b0' = rnorm(n.schools), 'b1' = rnorm(n.schools),
              'mu.int' = rnorm(1), 'sigma.int' = rlnorm(1), 'sigma' = rlnorm(1),
              'mu.slope' = rnorm(1), 'sigma.slope' = rlnorm(1))
     },
     'max_sector' = function() {
         list('b0' = rnorm(n.schools), 'b1' = rnorm(n.schools), 'b2' = rnorm(1),
              'mu.int' = rnorm(1), 'sigma.int' = rlnorm(1),'sigma' = rlnorm(1),
              'mu.slope' = rnorm(1), 'sigma.slope' = rlnorm(1))
     }
)

params <- list('fixed' = c('b0', 'b1'),
               'rint' = c('b0', 'b1', 'mu.int', 'sigma.int'),
               'rslope' = c('b0', 'b1', 'mu.slope', 'sigma.slope'),
               'maximal' = c('b0', 'b1', 'mu.int', 'sigma.int', 'mu.slope', 'sigma.slope'),
               'maxsector' = c('b0', 'b1', 'b2', 'delta', 'mu.int', 'sigma.int', 'mu.slope', 'sigma.slope'))