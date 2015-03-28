library('lme4')
library('rjags')
library('ggmcmc')
library('polspline')
source('setup.R')


###############################################
#### Setup the models and posterior samples
###############################################

# change the ids vector to change the models that get tested
ids <- c('rint', 'rslope', 'maximal')

# various differing models with respect to the random effects structure
models <- lapply(ids, function(id) get_model(paste0(id, '.txt'), inits[[id]], data))
names(models) <- ids

# samples from the posterior of the models
samples <- lapply(ids, function(id) get_samples(models[[id]], params[[id]]))
names(samples) <- ids


###############################################
# Model Comparison with respect to the random effects structure
###############################################

### DIC comparison
extract_dic <- function(dic_sample) sum(dic_sample$deviance) + sum(dic_sample$penalty)

dic_weights <- function(dic_samples, which = c('fixed', 'maximal')) {
    # see Wagenmakers (2004); same as AICw without 1/2 in exponent

    dics <- sapply(dic_samples, extract_dic)
    diff_dic <- dics - min(dics)

    exponents <- exp(-diff_dic)
    normalizer <- sum(exponents)

    names(exponents) <- names(dic_samples)
    w1 <- exponents[[which[1]]] / normalizer
    w2 <- exponents[[which[2]]] / normalizer
    w1 / w2
}


dic_samples <- lapply(models, dic.samples, n.iter = 1000)
names(dic_samples) <- ids

# the random intercept is infinitely more likely than the random slope model
dic_weights(dic_samples, which = c('rint', 'rslope'))
# the model with random intercept & slope is 33488160 more likely than the random intercept
dic_weights(dic_samples, which = c('maximal', 'rint'))


### Approximating Bayes factor via BIC
freq <- list('fixed' = lm(mathach ~ cses + factor(school), dat),
             'rint' = lmer(mathach ~ cses + (1 | school), dat, REML = FALSE),
             'rslope' = lmer(mathach ~ cses + (0 + cses | school), dat, REML = FALSE),
             'maximal' = lmer(mathach ~ cses + (cses | school), dat, REML = FALSE),
             'maxsector' = lmer(mathach ~ cses + sector + (cses | school), dat, REML = FALSE))

BF01_bic <- function(h1, h0) exp((BIC(h1) - BIC(h0)) / 2) # see Wagenmakers (2007)

# the random intercept is 3.24 * 10^288 more likely than the random slope model
BF01_bic(freq$rslope, freq$rint)
# the model with random intercept & slope is 311943 more likely than the random intercept
BF01_bic(freq$maximal, freq$rint) # varies


###############################################
# Testing the fixed effect *sector* using Savage-Dickey & BIC-BF
###############################################

savage_dickey <- function(samples, prior = dcauchy(0)) {
    fit.posterior <- logspline(samples)
    posterior <- dlogspline(0, fit.posterior)
    BF01 <- posterior / prior
    BF01
}

sector_model <- get_model('maximal_sector.txt', inits = inits$max_sector, data_sector)
sector_samples <- get_samples(sector_model, params$maxsector)
sector_delta <- sector_samples[, 321]

sector_bic <- BF01_bic(freq$maximal, freq$maxsector)
sector_savage <- 1 / savage_dickey(sector_delta) # varies