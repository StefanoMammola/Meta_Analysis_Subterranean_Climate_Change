###############################################################

## A global meta-analysis on the biological impacts of climate change in subterranean ecosystems
# Ilaria Vaccarelli, Raquel Colado, David Sanchez-Fernandez, Diana M. P. Galassi, Susana Pallarés, Mattia Di Cicco, Melissa B. Meierhofer, Elena Piano, Stefano Mammola

## ------------------------------------------------------------------------
# 'Conversion formulas used in the meta-analysis'
## ------------------------------------------------------------------------

# Analysis performed with R (v. R 4.1.0) and R studio (v. 1.4.1103)
# Authors (code): Stefano Mammola

# Conversion formulas used in the meta analysis

# Terminology:
# N = sample size
# df = degree of fredom
# r = Pearson's r

# Mean, SD, and Sample size -----------------------------------------------

# standardized mean-difference effect size (d) -------------------------------

d = 0.90
N_contr   = 11
N_treat   = 11

a = (N_contr + N_treat)^2 / (N_contr*N_treat)
r = d / sqrt(d * d + a)

# Conversion formulas for d:
# https://www.campbellcollaboration.org/escalc/html/EffectSizeCalculator-SMD1.php

# Spearman's correlation --------------------------------------------------

rho = 0.2
r = rho

# Tau ---------------------------------------------------------------------

tau = 0.2
r = sin(pi* tau / 2) 

# Regression analyses -----------------------------------------------------

# Used for LM, GLM, GLMM, GAM, GAMM, OLS

# ANOVA

F  = 5.46
df = 44
r = sqrt((F*F)/(F*F+df))

# Formula for converting estimated beta and SE to test statistic (t, Z)
beta = 0.3
SE = 0.02
test = beta/SE

#Formula for converting 95 % coefficient interval to SE
upper_limit = 0.45
lower_limit = 0.01
CI_95 = 3.92
CI_90 = 3.29 
CI_99 = 5.15

SE = (upper_limit -lower_limit) / CI_95 # Change CI as per above

# test statistic (e.g. z, wald-t)
N = 41
test = 1.96
r = test / sqrt(N)

# R^2, R-squared
R = 0.7
r = sqrt(R)

# Wilcoxon test -----------------------------------------------------------

Z = 2.3
N = 57
r = Z / sqrt(N)

# t-test ------------------------------------------------------------------

t = 0.54
N = 21
r = sqrt((t*t)/(t*t+N-2))
