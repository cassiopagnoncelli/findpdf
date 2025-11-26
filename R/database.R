#
# Probability [Distribution | Mass] Functions database.
#

# Pdf and pmf database.
pfDB <- data.frame(name = c(), pf = c(), discrete = c(), domain = c(), exotic = c())

pfParamsDB <- list()

#' Insert probability function into database
#'
#' Adds a distribution entry to the global pfDB and pfParamsDB structures.
#'
#' @param name Character; distribution name (e.g., "normal", "binomial")
#' @param pf Character; R function name for density/mass (e.g., "dnorm", "dbinom")
#' @param discrete Logical; TRUE for discrete distributions
#' @param domain Character; one of "REAL", "NONNEGATIVE", "NONPOSITIVE", "NEGATIVE"
#' @param params Data frame with columns: name, min, max, discrete, initial
#' @param exotic Logical; TRUE if distribution is exotic (default: TRUE)
#' @return NULL (modifies global pfDB and pfParamsDB)
#' @examples
#' \dontrun{
#' insertPF(
#'   "normal", "dnorm", FALSE, "REAL",
#'   data.frame(
#'     name = c("mean", "sd"), min = c(-Inf, 0),
#'     max = c(Inf, Inf), discrete = c(FALSE, FALSE),
#'     initial = c(0, 1)
#'   ), FALSE
#' )
#' }
insertPF <- function(name, pf, discrete, domain, params, exotic = TRUE) {
  row <- data.frame(name = name, pf = pf, discrete = discrete, domain = domain, exotic = exotic)
  pfDB <<- rbind(pfDB, row)
  pfParamsDB[[pf]] <<- params
}

#
# Functions.
#

# Continuous
insertPF(
  "normal", "dnorm", FALSE, "REAL",
  data.frame(
    name = c("mean", "sd"),
    min = c(-Inf, 0),
    max = c(Inf, Inf),
    discrete = c(FALSE, FALSE),
    initial = c(0, 1)
  ),
  FALSE
)

insertPF(
  "log-normal", "dlnorm", FALSE, "NONNEGATIVE",
  data.frame(
    name = c("mean", "sd"),
    min = c(0, 0),
    max = c(Inf, Inf),
    discrete = c(FALSE, FALSE),
    initial = c(1, 1)
  ),
  FALSE
)

insertPF(
  "beta", "dbeta", FALSE, "NONNEGATIVE",
  data.frame(
    name = c("shape", "scale"),
    min = c(0, 0),
    max = c(Inf, Inf),
    discrete = c(FALSE, FALSE),
    initial = c(1, 1)
  ),
  FALSE
)

insertPF(
  "gamma", "dgamma", FALSE, "NONNEGATIVE",
  data.frame(
    name = c("shape", "scale"),
    min = c(0, 0),
    max = c(Inf, Inf),
    discrete = c(FALSE, FALSE),
    initial = c(0, 1)
  ),
  FALSE
)

insertPF(
  "exponential", "dexp", FALSE, "NONNEGATIVE",
  data.frame(
    name = c("rate"),
    min = c(0),
    max = c(Inf),
    discrete = c(FALSE),
    initial = c(1)
  ),
  FALSE
)

# insertPF(
#   "f", "df", FALSE, "NONNEGATIVE",
#   data.frame(
#     name = c("df1", "df2"),
#     min = c(0, 0),
#     max = c(Inf, Inf),
#     discrete = c(TRUE, TRUE),
#     initial = c(50, 50)
#   ),
#   FALSE
# )

## NEEDS AMENDING a < b.
# insertPF('uniform', 'dunif', FALSE, 'REAL',
#         data.frame(
#           name=c('a', 'b'),
#           min=c(-Inf, -Inf),
#           max=c(Inf, Inf),
#           discrete=c(FALSE, FALSE),
#           initial=c(0, 0)),
#         FALSE)

insertPF(
  "weibull", "dweibull", FALSE, "NONNEGATIVE",
  data.frame(
    name = c("shape", "scale"),
    min = c(0, 0),
    max = c(Inf, Inf),
    discrete = c(FALSE, FALSE),
    initial = c(1, 1)
  ),
  FALSE
)

insertPF(
  "chi squared", "dchisq", TRUE, "NONNEGATIVE",
  data.frame(
    name = c("df"),
    min = c(0),
    max = c(Inf),
    discrete = c(TRUE),
    initial = c(100)
  ),
  FALSE
)

insertPF(
  "t", "dt", TRUE, "REAL",
  data.frame(
    name = c("df"),
    min = c(0),
    max = c(Inf),
    discrete = c(TRUE),
    initial = c(100)
  ),
  FALSE
)

# Discrete
insertPF(
  "binomial", "dbinom", TRUE, "NONNEGATIVE",
  data.frame(
    name = c("size", "prob"),
    min = c(0, 0),
    max = c(Inf, 1),
    discrete = c(TRUE, FALSE),
    initial = c(50, 0.5)
  ),
  FALSE
)

insertPF(
  "negative binomial", "dnbinom", TRUE, "NONNEGATIVE",
  data.frame(
    name = c("size", "prob"),
    min = c(0, 0),
    max = c(Inf, 1),
    discrete = c(TRUE, FALSE),
    initial = c(100, 0.5)
  ),
  FALSE
)

insertPF(
  "poisson", "dpois", TRUE, "NONNEGATIVE",
  data.frame(
    name = c("lambda"),
    min = c(0),
    max = c(Inf),
    discrete = c(FALSE),
    initial = c(100)
  ),
  FALSE
)

insertPF(
  "geometric", "dgeom", TRUE, "NONNEGATIVE",
  data.frame(
    name = c("prob"),
    min = c(0),
    max = c(1),
    discrete = c(FALSE),
    initial = c(0.5)
  ),
  FALSE
)

insertPF(
  "hypergeometric", "dhyper", TRUE, "NONNEGATIVE",
  data.frame(
    name = c("m", "n", "k"),
    min = c(0, 0, 0),
    max = c(Inf, Inf, Inf),
    discrete = c(TRUE, TRUE, TRUE),
    initial = c(10, 10, 10)
  ),
  FALSE
)
