server <- function(input, output, session) {

  ### Ovládání
  # Změny ovládání vlivem spojitosti
  observe({
    if (input$continuous) {
      enable("normality")
      # Vždy zapnout normalitu
      if (input$normality) {
        enable("mu")
        enable("sigma")
      }
    } else {
      disable("normality")
      # Vždy vypnout normalitu
      disable("mu")
      disable("sigma")
    }
  }) |>
    bindEvent(input$continuous, ignoreInit = TRUE)

  # Změna ovládání vlivem nezávislosti
  observe({
    if (input$dependent) {
      enable("correlation")
    } else {
      disable("correlation")
    }
  }) |>
    bindEvent(input$dependent, ignoreInit = TRUE)

  # Změny ovládání vlivem normality
  observe({
    if (input$normality) {
      enable("mu")
      enable("sigma")
    } else {
      disable("mu")
      disable("sigma")
    }
  }) |>
    bindEvent(input$normality, ignoreInit = TRUE)

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

  sample <- reactive({
    # TODO: ...

    rnorm(input$n, input$mu, input$sigma)
  }) |>
    bindEvent(input$generate)

  ### Panel 1 - Náhodný výběr
  output$sam_hs <- renderPlot({
    hist(sample(),
         main = "Histogram náhodného výběru",
         xlab = "Náhodný výběr", ylab = "Četnost")
  })

  output$sam_sw <- renderText({
    result <- capture.output(shapiro.test(sample()))

    paste(result[-1], collapse = "\n")
  })

  output$sam_char <- renderText({
    data <- sample()
    x <-
      c("Průměr:" = mean(data),
        "Rozptyl:" = var(data),
        "Šikmost:" = skewness(data),
        "Koeficient špičatosti:" = kurtosis(data)
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
  })
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
        "Rozptyl:" = var(data),
        "Šikmost:" = skewness(data),
        "Koeficient špičatosti:" = kurtosis(data)
      )

    paste(names(x), x, sep = " ", collapse = "\n")
  })
  # ----------------------------------------------------------------------------

}