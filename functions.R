
################################# diff in Weeks
pre_wk_diff <- function(given_wk, wk_list) {
  # Convert dates to Date objects
  diff <- round(given_wk - wk_list)

  preHol <- -max(diff[diff < 1])

  preHol
}

post_wk_diff <- function(given_wk, wk_list) {
  # Convert dates to Date objects
  # given_date <- as.Date(given_date, format = "%d-%m-%y")
  diff <- round(given_wk - wk_list)
  if (given_wk < 6){

    postHol <- given_wk # since the last week of the year is Christimas..

  } else {

    postHol <- min(diff[diff > -1])

  }
    # Return results
  postHol

  }








R2 <- function(model, data, k) {

  #k number of variables (no constant)

  # Extract the predicted values from the model
  predicted <- predict(model, newdata = data)

  # Calculate the mean of the dependent variable
  y_mean <- mean(data$Weekly_Sales)

  # Calculate the total sum of squares (TSS)
  tss <- sum((data$Weekly_Sales - y_mean)^2)

  # Calculate the residual sum of squares (RSS)
  rss <- sum((data$Weekly_Sales - predicted)^2)

  # Calculate R-squared
  n <- nrow(data)

  r_sq <- 1 - (rss / tss)
  r_sq_adj <- 1 - ((1 - r_sq)*(n - 1))/(n - k - 1)

  return(r_sq)
}
