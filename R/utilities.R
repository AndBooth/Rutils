
# This package contain utility functions that I have written and found useful whilst using R.

#' Are all values in a column NA?
#' @description Returns a logical value describing if all the values in the input are NA
#' @param x A vector
#' @return Logical
#' @examples
#' all_na(c(1,2,NA,3,4))
#'
#' # Use with purrr to test columns individually
#'
#' nadf <- data.frame(x = rep(NA, 3), y = c(1, 2, NA))
#' purrr::map_lgl(nadf, ~ all_na(.))
#'
#' @export

all_na <- function(x) {

  if (!is.vector(x) & !is.factor(x)) {
    stop("Input is not a column", call. = FALSE)
  }

  all(is.na(x))

}



#' Percent of values in a vector that are NA
#' @description Takes a vector and calculates the percent of values that are NA
#' @param x Vetor of values
#' @return Percent of values in x that are NA
#' @examples
#' percent_empty(c(1,2,NA,4))
#'
#' # Use with purrr to test columns in matrix, dataframe etc.
#'
#' nadf <- data.frame(x = rep(NA, 3), y = c(1, 2, NA))
#' purrr::map_dbl(nadf, ~ percent_empty(.))
#' @export
percent_empty <- function(x) {

  #label <- deparse(substitute(x))

  # Error if param x is not a vector type
  if (!is.vector(x)) {
    stop("Input '", label, "' is not of type: vector", call. = FALSE)
  }

  sum(is.na(x)) / length(x) * 100

}



#' Check if all values in a column are the same
#' @description Checks if all values in a column are the same, works for any type of data
#' @param x Vector of values
#' @param roundx Only use if data is numeric, rounds the calculated variance if a value is given
#' @details Be careful using the \code{roundx} argument. e.g. repeating_column(c(1, 1, 1.0001), roundx = 8) will
#' return TRUE.
#' @return Logical
#' @examples
#' repeating_column(rep(1, 5))
#'
#' repeating_column(c("dog", "dog", "dog", "cat"))
#'
#' # Use with purrr to test multiple columns
#'
#' testdf <- data.frame(x = rep(1, 5), y = c(1, 2, 1, 1, 1))
#' purrr::map_lgl(testdf, ~ repeating_column(.))
#' @export
repeating_column <- function(x,
                             roundx = NULL) {

  if (is.character(x) | is.factor(x)) {

    identical <- ifelse(length(unique(x)) == 1, T, F)

  } else {

    xvar <- var(x, na.rm = T)

    if (is.null(roundx)) {

      identical <- ifelse(xvar == 0, T, F)

    } else {

      xvar <- round(xvar, roundx)
      identical <- ifelse(xvar == 0, T, F)

    }
  }

  identical
}



#' Get random samples from a dataframe, without replacement
#' @description Takes \code{n} random sample from the dataframe \code{df} without replacement
#' @param df Dataframe from which to sample
#' @param n Integer value of number of samples to take
#' @return Dataframe with sampled rows
#' @examples
#' set.seed(1)
#'
#' test <- data.frame(x = seq(1, 10, 1), y = rnorm(10))
#' test_sample <- sample_df(test, 5)
#' @export
sample_df <- function(df, n) {

  ndf <- nrow(df)

  if (n > ndf) {
    stop("Cannot take a sample larger than the number of rows in 'df'",
         call. = FALSE)
  }

  idxs <- sample(ndf, n)
  df[idxs, ]
}


#' Order a dataframe by a given column
#' @description A simple wrapper round the base R \code{order} function allowing chaining with \code{\%>\%}
#' @param df the dataframe to order
#' @param x the column to order by
#' @param decreasing logical. Is the order increasing or decreasing?
#' @return An ordered dataframe
#' @examples
#' order_df(mtcars, ~ cyl)
#'
#' # Same operation can be done with the pipe operator
#'
#' mtcars %>% order_df(~ cyl)
#' @export
order_df <- function(df, x,
                     decreasing = FALSE) {

  column <- lazyeval::f_eval(x, df)

  df[order(column, decreasing = decreasing), ]

}

#' Calculate proportions within groups
#' @description Given a dataframe and one or more grouping variables this function will calculate the
#' proportion of group observations within groups
#' @param df dataframe containing the grouping variables
#' @param ... any number of grouping variables passed as string column names
#' @return dataframe with counts and proportions of groups within groups
#' @note the function uses the non standard evaluation of dplyr's \code{group_by} function \code{group_by_}
#' meaning that grouping variables should be passed as separate strings
#' @examples
#' group_proportions(mtcars, "hp", "cyl")
#' @export
group_proportions <- function(df, ...) {

  args <- c(...)

  if(!all(args %in% colnames(df))) {
    stop("one of grouping variables `", paste(args, collapse = "/"), "` not found in df", call. = FALSE)
  }

  grouped <- dplyr::group_by_(df, ...)
  counts <- dplyr::summarise(grouped, n = n())
  dplyr::mutate(counts, props = n / sum(n))

}

#' Spread multiple columns from long to wide format
#' @export
spread_multi <- function(data, key_col, ...,
                         fill = NA,
                         convert = TRUE,
                         drop = TRUE,
                         sep = "_") {

  # https://stackoverflow.com/questions/46009802/spread-multiple-columns-in-a-function

  key_quo <- rlang::enquo(key_col)
  val_quos <- rlang::quos(...)
  value_cols <- unname(tidyselect::vars_select(names(data), !!!val_quos))
  key_col <- unname(tidyselect::vars_select(names(data), !!key_quo))

  data %>%
    gather(key = ..var.., value = ..val.., !!!val_quos) %>%
    unite(col = ..grp.., c(key_col, "..var.."), sep = sep) %>%
    spread(key = ..grp.., value = ..val.., fill = fill, convert = convert,
           drop = drop, sep = NULL)

}
