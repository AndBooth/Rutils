


#' @export
days_of_week <- function(x,
                         first_day = "Monday") {

  days_c <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

  if(first_day != "Monday") {
    first_idx <- which(first_day == days_c)
    days <- c(days[first_idx:length(days)], days[1:(first_idx - 1)])
  }

  factor(weekdays(x), levels = days)

}



#' @export
months_of_year <- function(x,
                           first_month = "January") {

  months_c <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

  if(first_month != "January") {
    first_idx <- which(first_month == months_c)
    months_c <- c(months_c[first_idx:length(months_c)], months_c[1:(first_idx - 1)])
  }

  factor(months(x), levels = months_c)

}
