ui <- page_sidebar(
  lang = "cs",
  title = NULL,
  window_title = "Bootstrapováníčko",
  theme = bs_theme(version = 5,
                   bootswatch = "flatly") |>
    bs_add_rules(sass::sass_file("www/style.scss")),
  sidebar = sidebar(
    open = "always",
    width = "20vw",
    # Velikost výběry
    numericInput(inputId = "n",
                 label = "Velikost výběru",
                 value = 30, min = 1, max = 1000),
    hr(),
    # Spojitost
    checkboxInput(inputId = "continuous",
                  label = "Jsou data spojitá?",
                  value = TRUE),
    hr(),
    # Nezávislost
    checkboxInput(inputId = "dependent",
                  label = "Jsou data závislá?",
                  value = FALSE),
    disabled(numericInput(inputId = "correlation",
                          label = "Síla korelace",
                          value = 0.3, min = -1, max = 1)),
    hr(),
    # Normalita
    checkboxInput(inputId = "normality",
                  label = "Je splněna normalita?",
                  value = TRUE),
    numericInput(inputId = "mean",
                 label = "Střední hodnota",
                 value = 0, min = -Inf, max = Inf),
    numericInput(inputId = "sigma",
                 label = "Směrodatná odchylka",
                 value = 1, min = 1e-16, max = Inf),
    hr(),
    # Odlehlé hodnoty
    checkboxInput(inputId = "outliers",
                  label = "Jsou v datech odlehlé hodnoty?",
                  value = FALSE),
    disabled(numericInput(inputId = "outliers_n",
                          label = "Kolik generovat odlehlých hodnot?",
                          value = 1, min = 1, max = 30)),
    disabled(numericInput(inputId = "outliers_sigma",
                          label = "Jaký je maximální limit (kolik sigma)?",
                          value = 1, min = 1e-16, max = Inf)),
    hr(),
    actionBttn(
      inputId = "generate",
      label = "Generuj!",
      style = "stretch",
      color = "success"
    )
  ),
  shinyjs::useShinyjs(),
  tabsetPanel(
    tabPanel(
      title = "Generovaná data",
      plotOutput("out"),
      fluidRow(
        column(width = 6,
               verbatimTextOutput("shapirowilk")
        )
      )
    )
  )
)