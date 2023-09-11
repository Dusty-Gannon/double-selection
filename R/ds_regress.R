

function(y, X, focals, method = "backward", criterion = "aic"){

  # get the index for the control variables
  controls <- 1:ncol(X)[-c(1, focals)]

  # step 1 in double-selection
  m1 <- lm(y ~ X[, -focals] - 1)

  # step 2
  m2s <- vector(mode = "list", length = length(focals))
  for(i in 1:length(focals)){
    m2s[[i]] <- lm(X[, focals[i]] ~ X[, -focals] - 1)
  }

  # do the model selection for each
  all_list <- c(
    m1 = m1,
    m2s
  )

  if(sel_method == "aic"){
    includes <- lapply(
      all_list,
      aic_select
    )
  }

}
