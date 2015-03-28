library('lme4')
library('lattice')


# data from Baayen et al. (2008) to play around with
subj <- rep(c('s1', 's2', 's3'), each = 6)
item <- rep(c('w1', 'w2', 'w3'), times = 6)
soa <- rep(rep(c('long', 'short'), each = 3), times = 3)
rt <- c(466, 520, 502, 475, 494, 490, 516, 566, 577, 491, 544, 526,
        484, 529, 539, 470, 511, 528)
baayen <- data.frame(subj = subj, item = item, soa = soa, rt = rt)


dat <- read.table('../hierarchical/hsbdataset.txt', header = TRUE)
dat$school <- as.factor(as.numeric(as.factor(dat$school)))

# see Gelman (2006) about what hierarchical modeling can and cannot do!
fit.p <- lm(mathach ~ cses, dat) # pooling
fit.np <- lm(mathach ~ 0 + cses + factor(school), dat) # no pooling
fit <- lmer(mathach ~ cses + (cses | school), dat, REML = FALSE) # hierarchical


show_plot <- function() {
xyplot(mathach ~ cses | school, dat[dat$school %in% c(1:6, 21, 22), ],
       type=c("g", "r", "p"),
       col.line="red", lwd=1, 
       pch=16, strip = strip.custom(bg="grey96"),
       par.strip.text=list(cex=.8), layout=c(4, 2),
       ylab="Math Scores", xlab="SES (Centered around the school mean)")
}
