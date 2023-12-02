# Shiny
library(shiny)
library(shinyWidgets)
library(bslib)
library(shinyjs)
# Výpočty
library(boot)
# Funkce
generate_data <- function(continuous, normality, dependent, outliers,
                          n, mu, sigma, correlation, outliers_n) {
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
    for (i in seq(from = 1, to = length(data) - 1)) {
      data[i] <- data[i] + correlation * data[i + 1]
    }
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