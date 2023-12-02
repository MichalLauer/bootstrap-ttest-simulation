ui <- page_sidebar(
  lang = "cs",
  title = NULL,
  window_title = "Bootstrapováníčko",
  theme = bs_theme(version = 5,
                   bootswatch = "flatly") |>
    bs_add_rules(sass::sass_file("www/style.scss")),
  sidebar = sidebar(
    open = "always",
    # Velikost výběry
    numericInput(inputId = "n",
                 label = "Výběr (n)",
                 value = 30, min = 1, max = 1000),
    numericInput(inputId = "n_boot",
                 label = "Bootstrap (R)",
                 value = 100, min = 1, max = 1000),
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
                          step = 0.1,
                          value = 0.3, min = -1, max = 1)),
    hr(),
    # Normalita
    checkboxInput(inputId = "normality",
                  label = "Je splněna normalita?",
                  value = TRUE),
    fluidRow(
      column(width = 6,
             numericInput(inputId = "mu",
                          label = "μ",
                          value = 0, min = -Inf, max = Inf)),
      column(width = 6,
             numericInput(inputId = "sigma",
                          label = "σ",
                          value = 1, min = 1e-16, max = Inf))
    ),
    hr(),
    # Odlehlé hodnoty
    checkboxInput(inputId = "outliers",
                  label = "Existují odlehlé hodnoty?",
                  value = FALSE),
    disabled(numericInput(inputId = "outliers_n",
                          label = "Kolik jich je?",
                          value = 1, min = 1, max = 30)),
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
    # Náhodný výběr
    tabPanel(
      title = "Náhodný výběr",
      fluidRow(
        column(width = 10,
               plotOutput("sam_hs")
        ),
        column(width = 2,
               plotOutput("sam_bp")
        )
      ),
      fluidRow(
        column(width = 6,
               verbatimTextOutput("sam_sw")
        ),
        column(width = 6,
               verbatimTextOutput("sam_char")
        )
      ),
      fluidRow(
        column(width = 6,
               verbatimTextOutput("sam_tt")
        ),
        column(width = 6,
               verbatimTextOutput("sam_tt_sim")
        )
      )
    ),
    # Bootstrapování
    tabPanel(
      title = "Bootstrap",
      fluidRow(
        column(width = 12,
               plotOutput("boot_hs")
        )
      ),
      fluidRow(
        column(width = 6,
               verbatimTextOutput("boot_sw")
        ),
        column(width = 6,
               verbatimTextOutput("boot_char")
        )
      )
    )
  )
)