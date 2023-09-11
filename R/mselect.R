
#' Best subsets regression
#'
#' This is a wrapper to compute a 0/1 vector for inclusion of each candidate variable.
#' The underlying methodology is from the \code{leaps} package.
#'
#' @param y Numeric response vector.
#' @param X Model matrix (with a vector of 1's in the first column for the intercept term)
#' @param method Search method. Can be one of \code{c("backward", "forward", "exhaustive", "seqrep")}.
#' @param criterion Criterion to use to select final model. Can be one of
#' \code{c("adjr2", "aic", "bic", "cp")}
#' @param nbest Number of models to consider of each model size.
#'
#' @return A vector of 0's and 1's indicating which variables were selected.
#'
#'
mselect <- function(
    y, X, method = "backward",
    criterion = "aic", nbest = 3,
    focals = NULL
  ){

  # compute best subsets
  bss_reg <- leaps::regsubsets(
    x = X[, -1],
    y = y,
    method = method,
    nbest = nbest,
    force.in = focals
  )

  # create a summary
  bss_summary <- summary(bss_reg)

  # calculate aic, if necessary
  if(criterion == "aic"){
    mfits <- apply(
      bss_summary$which,
      MARGIN = 1,
      FUN = function(v){
        lm(y ~ X[, v] - 1)
      }
    )
    bss_summary$aic <- sapply(
      mfits,
      FUN = AIC
    )
  }

  # find the optimal model based on the chosen criterion
  if(criterion == "adjr2"){
    mod <- which.max(bss_summary[[criterion]])
  } else {
    mod <- which.min(bss_summary[[criterion]])
  }

  # return a vector of 0s and 1s for inclusion of each variable
  return(as.numeric(bss_summary$which[mod, ]))


}
