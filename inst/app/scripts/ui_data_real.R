wellPanel(
  h1("Load real data:"),
  ###########################################################################################
  fluidRow(
    column(width=1,
           br(),
           actionButton(inputId="i3",label=imageOutput("catpaw3",height="100%", width="100%"))
    ),
    column(width=2,
           h3("Load data")
    ),
    column(width=2,
           fileInput('file', 'in CSV File',
                     accept=c('text/csv',
                              'text/comma-separated-values,text/plain',
                              '.csv'))
    ),
    column(width=1,
           checkboxInput(inputId="header",
                         label = "Header",
                         value=TRUE)
    ),
    column(width=2,
           textInput(inputId="na.strings",
                         label = "Missing data",
                         value="NA")
    ),
    column(width=2,
           selectInput(inputId="sep",
                       label = "Column separator",
                       choices =c(";",".","tab",","),
                       selected = ";")
    ),
    column(width=2,
           selectInput(inputId="dec",
                       label = "Decimal separator",
                       choices =c(".",","),
                       selected = ".")
    )
  ),#fluidRow
  uiOutput("i3"),
  ###########################################################################################
  fluidRow(
    hr(),
    column(width=1,
           br(),
           actionButton(inputId="i4",label=imageOutput("catpaw4",height="100%", width="100%"))
    ),
    column(width=2,
           h3("Choose variables")
    ),
    column(width=3,
           selectInput(inputId="x",
                       label = "Variable x",
                       choices =c("x","y1","y2"),
                       selected ="x"),
           checkboxInput(inputId="x_is_date",
                         label="x is a date",
                         value=FALSE),
           textInput(inputId="date_format",
                     label="date format",
                     value="%d/%m/%Y")
    ),
    column(width=3,
           selectInput(inputId="y1",
                       label = HTML("Variable y<sub>1</sub>"),
                       choices=c("x","y1","y2"),
                       selected="y1"),
           checkboxInput(inputId="y2_exists",
                         label = HTML("Add Variable y<sub>2</sub>"),
                         value=FALSE)
    ),
    column(width=3,
           conditionalPanel(condition="input.y2_exists==true",
                            selectInput(inputId="y2", label = "Variable y2",
                                        choices =c("x","y1","y2"),
                                        selected ="y2")
                     )
    )
    ),#fluidRow
    uiOutput("i4"),
  ###########################################################################################
    fluidRow(
      hr(),
      column(width=1,
             br(),
             actionButton(inputId="i5",label=imageOutput("catpaw5",height="100%", width="100%"))
      ),
      column(width=2,
             h3("Choose step")
      ),
      column(width=3,
             textInput(inputId="step",
                       label=HTML("Step between x-values (&#916x)"),
                       value="1"),
             textOutput("n_output")
      )
    ),#fluidRow
  uiOutput("i5")
)#wellPanel
