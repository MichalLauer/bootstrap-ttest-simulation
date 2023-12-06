server <- function(input, output, session) {

  # Spojitost
  observe({
    # Data jsou spojitá
    if (input$continuous) {
      enable("normality")
    }
    # Data nejsou spojitá
    else {
      disable("normality")
    }
  }) |>
    bindEvent(input$continuous, input$outliers)

  output$generated_data <- renderText({
    text <- data_info(continuous = input$continuous,
                      normality = input$normality,
                      outliers = input$outliers,
                      n = input$n)
    paste(text, collapse = "\n")
  }) |>
    bindEvent(input$generate)

  # ----------------------------------------------------------------------------


  # Generování dat
  current_sample <-
    reactive({
      generate_data(continuous = input$continuous,
                    normality = input$normality,
                    outliers = input$outliers,
                    n = input$n)
    }) |>
    bindEvent(input$generate)

  ### Panel 1 - Náhodný výběr
  output$ttest_hs <- renderPlot({
    hist(current_sample(),
         main = "Histogram náhodného výběru",
         xlab = "Náhodný výběr", ylab = "Četnost")
  }) |>
    bindEvent(current_sample())

  output$ttest_bp <- renderPlot({
    boxplot(current_sample())
  }) |>
    bindEvent(current_sample())

  output$ttest_sw <- renderText({
    res <- capture.output(shapiro.test(current_sample()))[-1]
    paste(res, collapse = "\n")
  }) |>
    bindEvent(current_sample())

  output$ttest_ttest <- renderText({
    paste(capture.output(t.test(x = current_sample(), mu = 10))[2:5], collapse = "\n")
  }) |>
    bindEvent(current_sample())

  ttest_simulation <-
    reactive({
      R <- input$R
      pvals1 <- numeric(R)
      pvals2 <- numeric(R)
      for (i in seq_len(R)) {
        data <- generate_data(continuous = input$continuous,
                              normality = input$normality,
                              outliers = input$outliers,
                              n = input$n)
        pvals1[i] <- t.test(x = data, mu = 10)$p.value
        pvals2[i] <- t.test(x = data, mu = 11)$p.value
      }
      list(pvals1 = pvals1,
           pvals2 = pvals2,
           type_I = mean(pvals1 <= 0.05),
           type_II = mean(pvals2 >= 0.05),
           power = 1 - mean(pvals2 >= 0.05)
      )
    }) |>
    bindEvent(input$generate)

  output$ttest_simulation <- renderText({
    sims <- ttest_simulation()
    x <-
      c("Počet simulací:" = input$R,
        "Chyba I. druhu:" = sims$type_I,
        "Chyba II. druhu (μ = 11):" = sims$type_II,
        "Síla testu:" = sims$power
      )

    paste(names(x), x, sep = " ", collapse = "\n")
  }) |>
    bindEvent(input$generate)


  output$ttest_chars <- renderText({
    data <- current_sample()
    x <-
      c("Odhadnutý průměr:" = mean(data),
        "Rozptyl průměru:" = var(data) / length(data),
        "Směr. Odchyl. průměru:" = sd(data) / sqrt(length(data)))

    paste(names(x), x, sep = " ", collapse = "\n")
  }) |>
    bindEvent(current_sample())
  # ----------------------------------------------------------------------------

  bootstrap_nonparam <- reactive({
    data <- current_sample()
    theta <- generate_nonboot(input$R, data)

    list(theta = theta,
         theta_boot = mean(theta),
         boot_se = sd(theta),
         bias = mean(theta) - mean(data),
         CI = c(mean(theta) - qnorm(0.975)*sd(theta),
                mean(theta) + qnorm(0.975)*sd(theta)))
  }) |>
    bindEvent(input$generate)

  ### Panel 2 - Neparametrický bootstrap
  output$nonboot_hs <- renderPlot({
    hist(bootstrap_nonparam()$theta,
         main = "Histogram bootstrapovaných průměrů",
         xlab = "Průměry", ylab = "Četnost")
  })

  output$nonboot_bp <- renderPlot({
    boxplot(bootstrap_nonparam()$theta)
  })

  output$nonboot_sw <- renderText({
    res <- capture.output(shapiro.test(bootstrap_nonparam()$theta))[-1]
    paste(res, collapse = "\n")
  })

  observe({
    R <- input$R
    is_in <- numeric(R)
    # browser()
    for (i in seq_len(R)) {
      data <- generate_nonboot(R, current_sample())
      ci_lower <- mean(data) - qnorm(0.975)*sd(data)
      ci_upper <- mean(data) + qnorm(0.975)*sd(data)
      is_in[i] <- ci_lower <= 10 & 10 <= ci_upper
    }
  }) |>
    bindEvent(input$generate)

  output$nonboot_char <- renderText({
    x <- bootstrap_nonparam()[-1]
    paste(names(x), x, sep = ": ", collapse = "\n")
  })
  # ----------------------------------------------------------------------------

  bootstrap_param <- reactive({
    data <- current_sample()
    theta <- generate_parboot(input$R, data)

    list(theta = theta,
         theta_boot = mean(theta),
         boot_se = sd(theta),
         bias = mean(theta) - mean(data),
         CI = c(mean(theta) - qnorm(0.975)*sd(theta),
                mean(theta) + qnorm(0.975)*sd(theta)))
  }) |>
    bindEvent(input$generate)

  ### Panel 3 - Parametrický bootstrap
  output$parboot_hs <- renderPlot({
    hist(bootstrap_param()$theta,
         main = "Histogram bootstrapovaných průměrů",
         xlab = "Průměry", ylab = "Četnost")
  })

  output$parboot_bp <- renderPlot({
    boxplot(bootstrap_param()$theta)
  })

  parboot_sw <-
    reactive(capture.output(shapiro.test(bootstrap_param()$theta))[-1]) |>
    bindEvent(current_sample())
  output$parboot_sw <- renderText({
    paste(parboot_sw(), collapse = "\n")
  })

  output$parboot_char <- renderText({
    x <- bootstrap_param()[-1]
    paste(names(x), x, sep = ": ", collapse = "\n")
  })
  # ----------------------------------------------------------------------------



  ### Panel 4 - Porovnání
  output$distributions <- renderPlot({
    data <- current_sample()
    theta <- bootstrap_nonparam()$theta
    theta2 <- bootstrap_param()$theta

    df <- tibble(
      Metoda = c(rep("Náhodný výběr", length(data)),
                 rep("Neparametrický bootstrap", length(theta)),
                 rep("Parametrický bootstrap", length(theta2))),
      Data = c(data, theta, theta2))

    # browser()
    limit <- max(abs(max(df$Data) - mean(df$Data)), abs(min(df$Data) - mean(df$Data))) + 1
    limit <- ceiling(c(mean(data) - limit, mean(data) + limit))

    df |>
      ggplot(aes(x = Data, fill = Metoda)) +
      geom_density(alpha = 0.5) +
      geom_vline(xintercept = 10, linetype = "dashed") +
      scale_x_continuous(limits = limit) +
      theme_minimal() +
      theme(legend.position = "top")
  })

  output$comparison <- render_gt({
    s <- current_sample()
    nb <- bootstrap_nonparam()
    pb <- bootstrap_param()

    tribble(
      ~"Statistika",  ~"Náhodný výběr",     ~"Neparametrický bootstrap", ~"Parametrický bootstrap",
      "Odhad",        mean(s),               nb$theta_boot,              pb$theta_boot,
      "Rozptyl",      var(s)/length(s),      nb$boot_se^2,               pb$boot_se^2,
      "Směr. odch.",  sd(s)/sqrt(length(s)), nb$boot_se,                 pb$boot_se,
    ) |>
      gt()

  })

}
