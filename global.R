# Shiny
library(shiny)
library(shinyWidgets)
library(bslib)
library(shinyjs)
# Výpočty
library(boot)
# Funkce
generate_data <- function(continuous, normality, dependent, outliers,
                          n, mu, sigma = mu, correlation, outliers_n) {
  if (continuous) {
    # Spojitá data
    if (normality) {
      # Spojitá data - normální
      data <- rnorm(n = n, mean = mu, sd = sigma)
    } else {
      # Spojití data - lognormální
      data <- rlnorm(n = n, meanlog = log(mu), sdlog = log(sigma))
    }
  } else {
    # Nespojití data - Poissonovo
    data <- rpois(n = n, lambda = mu)
  }
  # Přidání závislosti
  if (dependent) {
    phi <- correlation  # Correlation coefficient
    sigma <- sqrt((1 - phi^2) * sigma^2)
    data <- arima.sim(model = list(ar = phi), n = n, sd = sigma)
  }
  # Přidání odlehlých hodnot
  if (outliers) {
    indexes <- sample.int(length(data), size = outliers_n)
    for (index in indexes) {
      direction <- ifelse(data[index] < mean(data), -1, 1)
      data[index] <- data[index] + 3 * IQR(data) * direction
    }
  }

  data
}