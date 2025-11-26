#
# Probability [Distribution | Mass] Functions database.
#

# Pdf and pmf database.
pfDB <- data.frame(name=c(), pf=c(), discrete=c(), domain=c(), exotic=c())

pfParamsDB <- list()

# Database manipulation.
insertPF <- function(name, pf, discrete, domain, params, exotic=TRUE) {
  row <- data.frame(name=name, pf=pf, discrete=discrete, domain=domain, exotic=exotic)
  assign('pfDB', rbind(pfDB, row), envir=.GlobalEnv)
  assign(paste("pfParamsDB$", pf, sep=""), params, envir=.GlobalEnv)
}

inf <- Inf  # 2^16

#
# Functions.
#

# Continuous
insertPF('normal', 'dnorm', FALSE, 'REAL', 
         data.frame(
           name=c('mean', 'sd'),
           min=c(-inf, 0),
           max=c(inf, inf),
           discrete=c(FALSE, FALSE),
           initial=c(0, 1)),
         FALSE)

insertPF('log-normal', 'dlnorm', FALSE, 'NONNEGATIVE',
         data.frame(
           name=c('mean', 'sd'),
           min=c(0, 0),
           max=c(inf, inf),
           discrete=c(FALSE, FALSE),
           initial=c(1, 1)),
         FALSE)

insertPF('beta', 'dbeta', FALSE, 'NONNEGATIVE', 
         data.frame(
           name=c('shape', 'scale'),
           min=c(0, 0),
           max=c(inf, inf),
           discrete=c(FALSE, FALSE),
           initial=c(1, 1)),
         FALSE)

insertPF('gamma', 'dgamma', FALSE, 'NONNEGATIVE',
         data.frame(
           name=c('shape', 'scale'),
           min=c(0, 0),
           max=c(inf, inf),
           discrete=c(FALSE, FALSE),
           initial=c(0, 1)),
         FALSE)

insertPF('exponential', 'dexp', FALSE, 'NONNEGATIVE', 
         data.frame(
           name=c('rate'),
           min=c(0),
           max=c(inf),
           discrete=c(FALSE),
           initial=c(1)),
         FALSE)

insertPF('f', 'df', FALSE, 'NONNEGATIVE', 
         data.frame(
           name=c('df1', 'df2'),
           min=c(0, 0),
           max=c(inf, inf),
           discrete=c(TRUE, TRUE),
           initial=c(50, 50)),
         FALSE)

## NEEDS AMENDING a < b.
#insertPF('uniform', 'dunif', FALSE, 'REAL', 
#         data.frame(
#           name=c('a', 'b'),
#           min=c(-inf, -inf),
#           max=c(inf, inf),
#           discrete=c(FALSE, FALSE),
#           initial=c(0, 0)),
#         FALSE)

insertPF('weibull', 'dweibull', FALSE, 'NONNEGATIVE', 
         data.frame(
           name=c('shape', 'scale'),
           min=c(0, 0),
           max=c(inf, inf),
           discrete=c(FALSE, FALSE),
           initial=c(1, 1)),
         FALSE)

insertPF('chi squared', 'dchisq', TRUE, 'NONNEGATIVE',
         data.frame(
           name=c('df'),
           min=c(0),
           max=c(inf),
           discrete=c(TRUE),
           initial=c(100)),
         FALSE)

insertPF('t', 'dt', TRUE, 'REAL',
         data.frame(
           name=c('df'),
           min=c(0),
           max=c(inf),
           discrete=c(TRUE),
           initial=c(100)),
         FALSE)

# Discrete
insertPF('binomial', 'dbinom', TRUE, 'NONNEGATIVE',
         data.frame(
           name=c('size', 'prob'),
           min=c(0, 0),
           max=c(inf, 1),
           discrete=c(TRUE, FALSE),
           initial=c(50, 0.5)),
         FALSE)

insertPF('negative binomial', 'dnbinom', TRUE, 'NONNEGATIVE',
         data.frame(
           name=c('size', 'prob'),
           min=c(0, 0),
           max=c(inf, 1),
           discrete=c(TRUE, FALSE),
           initial=c(100, 0.5)),
         FALSE)

insertPF('poisson', 'dpois', TRUE, 'NONNEGATIVE',
         data.frame(
           name=c('lambda'),
           min=c(0),
           max=c(inf),
           discrete=c(FALSE),
           initial=c(100)),
         FALSE)

insertPF('geometric', 'dgeom', TRUE, 'NONNEGATIVE',
         data.frame(
           name=c('prob'),
           min=c(0),
           max=c(1),
           discrete=c(FALSE),
           initial=c(0.5)),
         FALSE)

insertPF('hypergeometric', 'dhyper', TRUE, 'NONNEGATIVE',
         data.frame(
           name=c('m', 'n', 'k'),
           min=c(0, 0, 0),
           max=c(inf, inf, inf),
           discrete=c(TRUE, TRUE, TRUE),
           initial=c(10, 10, 10)),
         FALSE)
