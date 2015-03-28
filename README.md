# Hierarchical Bayesian Modeling
Materials for the presentation given at the 2015 Erasmus IP Seminar on Mathematical Psychology. Group work by Muriel Lawerman, Alexandra
Sarafoglou, Leander De Schutter and myself.

## Structure
We had an appendix that we did not present at the seminar (for time reasons). I have worked it into the main presentation, so it makes more sense. The hierarchical models were compared using DIC weights, BIC Bayes factor approximations (using _lme4_) and Savage-Dickey
for a fixed effect. The code can be found in **code/comparison/hierarchical/**. In **code/comparison/sanity/** you find **notes.R** which has the example data
from Baayen et al. (2008) and looks at pooling, no pooling and partial pooling (hierarchical). **titanic.R** is some random code doing a logistic regression
on titanic survival data. **ttest/savage_dickey.R** is adapted from Wagenmakers et al. (2010) and doing a hierarchical t-test.


### Suggested readings

- Baayen et al. (2008)
- Gelman (2006)
- Rouder & Morey (in press)

see **bib/** for all references.


### Disclaimer
We're all pretty new to hierarchical modeling. As for the code: I am accountable for any errors.
If you find any, please drop me an email.
