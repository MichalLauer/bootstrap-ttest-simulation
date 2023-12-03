server <- function(input, output, session) {

  ### Ovládání
  # Změny ovládání vlivem spojitosti
  # TODO: Změnit minimální hodnotu pro mu kvůli Poissonovu rozdělení
  observe({
    if (input$continuous) {
      enable("normality")
      enable("sigma")
    } else {
      disable("normality")
      disable("sigma")
    }
  }) |>
    bindEvent(input$continuous, ignoreInit = TRUE)

  # Změny ovládání vlivem velikosti vzorku
  observe({
    updateNumericInput(inputId = "outliers_n",
                       value = min(input$n, input$outliers_n),
                       max = input$n)
  }) |>
    bindEvent(input$n)

  # Změny ovládání vlivem odlehlých hodnot
  observe({
    if (input$outliers) {
      enable("outliers_n")
      enable("outliers_sigma")
    } else {
      disable("outliers_n")
      disable("outliers_sigma")
    }
  }) |>
    bindEvent(input$outliers, ignoreInit = TRUE)
  # ----------------------------------------------------------------------------


  # Generování dat
  sample <-
    reactive({
      generate_data(continuous = input$continuous,
                    normality = input$normality,
                    outliers = input$outliers,
                    n = input$n,
                    mu = input$mu,
                    sigma = input$sigma,
                    outliers_n = input$outliers_n)
    }) |>
    bindEvent(input$generate)

  ### Panel 1 - Náhodný výběr
  output$sam_hs <- renderPlot({
    hist(sample(),
         main = "Histogram náhodného výběru",
         xlab = "Náhodný výběr", ylab = "Četnost")
  })

  output$sam_bp <- renderPlot({
    boxplot(sample())
  })

  output$sam_sw <- renderText({
    result <- capture.output(shapiro.test(sample()))

    paste(result[-1], collapse = "\n")
  })

  output$sam_char <- renderText({
    data <- sample()
    x <-
      c("Průměr:" = mean(data),
        "Směrodatná odchylka:" = sd(data),
        "Rozptyl:" = var(data),
        "Počet odlehlých hodnot:" = sum(data < quantile(data, probs = 0.25) - 1.5*IQR(data) |
                                          data > quantile(data, probs = 0.75) + 1.5*IQR(data))
      )

    paste(names(x), x, sep = " ", collapse = "\n")
  })

  output$sam_tt <- renderText({
    tt <- t.test(x = sample(),
                 mu = input$mu)
    x <-
      c("p-hodnota:" = tt$p.value,
        "95% CI:" = paste0("(", paste0(tt$conf.int, collapse = ", "), ")"),
        "Výsledek:" = ifelse(tt$p.value < 0.05, "Zamítáme H0", "Nemůžeme zamítnout H0")
      )

    paste(names(x), x, sep = " ", collapse = "\n")
  }) |>
    bindEvent(input$generate)

  output$sam_tt_sim <- renderText({
    R <- 2
    pvals1 <- numeric(R)
    pvals2 <- numeric(R)
    mu2 <- input$mu + 1
    for (i in seq_len(R)) {
      data <- generate_data(continuous = input$continuous,
                            normality = input$normality,
                            outliers = input$outliers,
                            n = input$n,
                            mu = input$mu,
                            sigma = input$sigma,
                            outliers_n = input$outliers_n)
      pvals1[i] <- t.test(x = data, mu = input$mu)$p.value
      pvals2[i] <- t.test(x = data, mu = mu2)$p.value
    }

    x <-
      c("Počet simulací:" = R,
        "Chyba I. druhu:" = mean(pvals1 <= 0.05),
        mean(pvals2 >= 0.05),
        "Síla testu:" = 1 - mean(pvals2 >= 0.05)
      )
    names(x)[3] <- paste0("Chyba II. druhu (μ = ", mu2, ")")

    paste(names(x), x, sep = " ", collapse = "\n")
  }) |>
    bindEvent(input$generate)
  # ----------------------------------------------------------------------------

  bootstrap <- reactive({
    boot(data = sample(),
         statistic = function(data, i) mean(data[i]),
         R = input$n_boot)
  }) |>
    bindEvent(sample())

  ### Panel 2 - Bootstrap
  output$boot_hs <- renderPlot({
    # boot <-
    hist(bootstrap()$t,
         main = "Histogram bootstrapovaných průměrů",
         xlab = "Průměry", ylab = "Četnost")
  })

  output$boot_sw <- renderText({
    result <- capture.output(shapiro.test(bootstrap()$t))

    paste(result[-1], collapse = "\n")
  })

  output$boot_char <- renderText({
    data <- bootstrap()$t
    x <-
      c("Průměr:" = mean(data),
        "Rozptyl:" = var(data)
      )

    paste(names(x), x, sep = " ", collapse = "\n")
  })
  # ----------------------------------------------------------------------------

}