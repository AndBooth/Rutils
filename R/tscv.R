

#' @export
TimeSeriesSplitValidation <- function(n_samples, K, test_size = NULL) {

  fold_idxs <- vector(mode = "list", length = K)

  if(is.null(test_size)) {
    test <- round(n_samples / (K + 1))
    for(i in 1:K) {
      train <- i * (n_samples) / (K + 1) + n_samples %% (K + 1)
      fold_idxs[[i]]$train_idxs <- 1:round(train)
      if(i != K){
        fold_idxs[[i]]$test_idxs <- (round(train) + 1):(round(train) + test)
      } else {
        fold_idxs[[i]]$test_idxs <- (round(train) + 1):n_samples
      }
    }
  } else {
    test <- test_size
    max_idxs <- sapply(1:K, function(x) round(n_samples * x/K))
    for(i in seq_along(max_idxs)) {
      train <- max_idxs[i] - test_size
      fold_idxs[[i]]$train_idxs <- 1:train
      fold_idxs[[i]]$test_idxs <- (max_idxs[i] - test_size + 1):max_idxs[i]
    }
  }

  fold_idxs
}

#' @export
TimeSeriesWalkForwardValidation <- function(n_samples, min_obs, h, expanding = T, sliding_max_size) {

  K <- ifelse(h == 1,
              n_samples - min_obs,
              ceiling((n_samples - min_obs) / h))

  fold_idxs <- vector(mode = "list", length = K)

  for(i in 1:K){

    train <- ifelse(i == 1, min_obs, train + h)

    if(expanding){
      fold_idxs[[i]]$train_idxs <- 1:train
    } else {
      fold_idxs[[i]]$train_idxs <- max(c(1, train - sliding_max_size + 1)):train
    }

    fold_idxs[[i]]$test_idxs <- if (h == 1) {
      train + 1
    } else {
      if (i == K) {
        (train + 1):n_samples
      } else {
        (train + 1):(train + h)
      }
    }
  }

  fold_idxs
}
