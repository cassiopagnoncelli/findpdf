#
# Find the distribution of a given set.
#
# Idea:
# 1. Extend the dataset with bootstrap if it is too small
# 2. Classify the data (eg. real or nonnegative or positive, integer or continuous
# 3. Find the best parameters for each distribution
# 4. List the distributions with the best fitting ranked by the most likely distributions
#

# TODO
# 1. include parameters restrictions in rmse, eg. a < b in runif.
# 2. solve the warnings().
#
#options(error = recover)

# Find the most fit distributions for the given data.
findpdf <- function(x, include.exotics=FALSE, remove.na=TRUE, search.combinations=TRUE) {
  # Key data information.
  ds <- data_summary(x)
  
  # List pmf/pdf candidates.
  candidates <- pfDB[pfDB$discrete == ds$is_discrete,]
  if (!include.exotics)
    candidates <- candidates[candidates$exotic == FALSE,]
  
  # Estimate probability density/mass function.
  if (ds$is_discrete) {                             ################ FIXME ##############
    f <- hist(x, breaks=(min(x)-0.5):(max(x)+0.5), plot=F)
    f.x <- f$mids
    f.y <- f$density
  } else {
    f <- density(x, from=min(x), to=max(x))
    f.x <- f$x
    f.y <- f$y
  }
  
  # Perform optimization across candidates
  ranking <- data.frame(pf=c(), error=c())
  best_params <- list()
  for (i in 1:nrow(candidates)) {
    candidate <- candidates[i,]
    
    cat(paste("Testing", candidate$name, "distribution... "))
    
    params_meta <- get(paste('pfParamsDB$', candidate$pf, sep=''))
    
    conv.params <- cmpfun(function(params) {
      b <- as.logical(params_meta$discrete)
      for (i in 1:length(params))
        if (b[i])
          params[i] <- round(params[i])
      
      return(params)
    })
    
    rmse <- cmpfun(function(params) {
      sqrt(sum((do.call(
        as.character(candidate$pf),
        append(list(f.x), as.list(conv.params(params)))) - f.y)^2))
    })
    
    if (sum(params_meta$discrete) > 0) {
      o <- GenSA(params_meta$initial, rmse,
                 lower=params_meta$min, upper=params_meta$max,
                 control=list(maxit=3000))
    } else {
      o <- optim(params_meta$initial, rmse,
                 lower=params_meta$min, upper=params_meta$max, method='L-BFGS-B',
                 control=list())
    }
    
    ranking <- rbind(ranking, data.frame(pf=candidate$pf, error=o$value))
    best_params[[as.character(candidate$pf)]] <- conv.params(o$par)
    
    if (o$counts[1] <= 2)
      print(o)
    
    cat("OK\n")
  }
  
  ranking <- ranking[order(ranking$error),]
  
  list(
    params = best_params,
    ranking = ranking
  )
}
