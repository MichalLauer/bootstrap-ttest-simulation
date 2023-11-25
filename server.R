server <- function(input, output, session) {
  # Změny ovládání vlivem spojitosti
  observe({
    if (input$continuous) {
      enable("normality")
      # Vždy zapnout normalitu
      if (input$normality) {
        enable("mu")
        enable("sigma2")
      }
    } else {
      disable("normality")
      # Vždy vypnout normalitu
      disable("mu")
      disable("sigma2")
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
      enable("sigma2")
    } else {
      disable("mu")
      disable("sigma2")
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

  # Generování dat
  population <-
    reactive({
      # TODO...

      rnorm(n = input$n, mean = input$mean, sd = input$sigma)
    }) |>
    bindEvent(input$generate)

  output$out <- renderPlot({
    hist(population())
  })

  output$shapirowilk <- renderText({
    t <- shapiro.test(population())

    t
  })
}