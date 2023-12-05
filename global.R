# Shiny
library(shiny)
library(shinyWidgets)
library(bslib)
library(shinyjs)
# Výpočty
library(boot)
# Funkce
generate_data <- function(continuous, normality, outliers,
                          n, mu, sigma = mu, outliers_n) {
  if (continuous) {
    # Spojitá data
    if (normality) {
      # Spojitá data - normální
      data <- rnorm(n, mean = mu, sd = sigma)
    } else {
      # Spojití data - lognormální
      data <- rlnorm(n = n, meanlog = log(mu), sdlog = log(sigma))
    }
  } else {
    # Nespojití data - Poissonovo
    data <- rpois(n = n, lambda = mu)
  }

  # Přidání odlehlých hodnot
  if (outliers) {
    indexes <- sample.int(length(data), size = outliers_n)
    data[indexes] <- data[indexes] + 3 * IQR(data)
  }

  data
}