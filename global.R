# Shiny
library(shiny)
library(shinyWidgets)
library(bslib)
library(shinyjs)
library(gt)
library(dplyr)
library(ggplot2)
# Funkce
generate_data <- function(continuous, normality, outliers, n) {
  mu <- 10
  sigma <- 5
  out_n <- round(n*0.10)

  if (continuous) {
    # Spojitá data
    if (normality) {
      # Spojitá data - normální
      data <- rnorm(n, mean = mu, sd = sigma)
    } else {
      # Spojití data - lognormální
      # https://msalganik.wordpress.com/2017/01/21/making-sense-of-the-rlnorm-function-in-r/comment-page-1/
      location <- log(mu^2 / sqrt(sigma^2 + mu^2))
      shape <- sqrt(log(1 + (sigma^2 / mu^2)))
      data <- rlnorm(n = n, meanlog = location, sdlog = shape)
    }
  } else {
    # Nespojití data - Poissonovo
    data <- rbinom(n = n, size = 25, prob = .4)
  }

  # Přidání odlehlých hodnot
  if (outliers) {
    base_mean <- mean(data)
    data <- c(head(data, n = n - out_n), data[(n - out_n + 1):n] * 1.4)
    data <- data - mean(data) + base_mean
  }

  data
}

data_info <- function(continuous, normality, outliers, n) {
  distr <- "Rozdělení: "
  params <- "Parametry: "
  out <- 0

  if (continuous) {
    # Spojitá data
    if (normality) {
      # Spojitá data - normální
      distr <- "Normal"
      params <- "- µ = 10\n - σ = 5"
    } else {
      # Spojití data - lognormální
      distr <- "Lognormal"
      params <- "- µ = 10\n - σ = 5"
    }
  } else {
    # Nespojití data - Poissonovo
    distr <- "Binomial"
    params <- "- n = 25\n - p = 0.4"
  }

  # Přidání odlehlých hodnot
  if (outliers) {
    out <- round(n*0.10)
  }

  list(distr = paste("Rozdělení:", distr),
       params = paste("Parametry:\n", params),
       outliers = paste("zvětšeno o 40%:\n - ", out, "hodnot"))
}

generate_nonboot <- function(R, data) {
  theta <- numeric(R)
  for (i in seq_along(theta)) {
    theta[i] <- mean(sample(data, size = length(data), replace = T))
  }
  theta
}

generate_parboot <- function(R, data) {
  theta <- numeric(R)
  for (i in seq_along(theta)) {
    theta[i] <- mean(rnorm(length(data), mean = mean(data), sd = sd(data)))
  }
  theta
}