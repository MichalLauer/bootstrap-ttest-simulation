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
    numericInput(inputId = "R",
                 label = "Bootstrap (R)",
                 value = 100, min = 1, max = 1000),
    hr(),
    # Spojitost
    checkboxInput(inputId = "continuous",
                  label = "Jsou data spojitá?",
                  value = TRUE),
    # Normalita
    checkboxInput(inputId = "normality",
                  label = "Je splněna normalita?",
                  value = TRUE),
    # Odlehlé hodnoty
    checkboxInput(inputId = "outliers",
                  label = "Existují odlehlé hodnoty?",
                  value = FALSE),
    hr(),
    actionBttn(
      inputId = "generate",
      label = "Generuj!",
      style = "stretch",
      color = "success"
    ),
    hr(),
    verbatimTextOutput("generated_data")
  ),
  shinyjs::useShinyjs(),
  tabsetPanel(
    # Náhodný výběr
    tabPanel(
      title = "Náhodný výběr",
      fluidRow(
        column(width = 10,
               plotOutput("ttest_hs")
        ),
        column(width = 2,
               plotOutput("ttest_bp")
        )
      ),
      fluidRow(
        column(width = 6,
               verbatimTextOutput("ttest_sw")
        ),
        column(width = 6,
               verbatimTextOutput("ttest_ttest")
        )
      ),
      fluidRow(
        column(width = 6,
               verbatimTextOutput("ttest_simulation")
        ),
        column(width = 6,
               verbatimTextOutput("ttest_chars")
        )
      )
    ),
    # Bootstrapování
    tabPanel(
      title = "Neparametrický Bootstrap",
      fluidRow(
        column(width = 10,
               plotOutput("nonboot_hs")
        ),
        column(width = 2,
               plotOutput("nonboot_bp")
        )
      ),
      fluidRow(
        column(width = 6,
               verbatimTextOutput("nonboot_sw")
        ),
        column(width = 6,
               verbatimTextOutput("nonboot_char")
        )
      ),
      fluidRow(
        column(width = 6,
               verbatimTextOutput("nonboot_simulation")
        )
      )
    ),
    # Bootstrapování
    tabPanel(
      title = "Parametrický Bootstrap",
      fluidRow(
        column(width = 10,
               plotOutput("parboot_hs")
        ),
        column(width = 2,
               plotOutput("parboot_bp")
        )
      ),
      fluidRow(
        column(width = 6,
               verbatimTextOutput("parboot_sw")
        ),
        column(width = 6,
               verbatimTextOutput("parboot_char")
        )
      ),
      fluidRow(
        column(width = 6,
               verbatimTextOutput("parboot_sim")
        )
      )
    ),
    tabPanel(
      title = "Porovnání",
      plotOutput("distributions"),
      gt_output("comparison")
    )
  )
)