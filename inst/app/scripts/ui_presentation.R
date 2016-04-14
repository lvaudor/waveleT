tabPanel(title="INFO",
         icon=icon("info",lib="font-awesome"),
         fluidRow(
            column(width=4,offset=1,
                   br(),br(),br(),
                   imageOutput("mylogo",height="100%", width="100%")
            ),
            column(width=7,
                   h2("The Wavelet ToolKat 1.1"),
                   h4("L. Vaudor, ISIG, UMR 5600"),
                   h5("A set of tools for the analysis of series through wavelet transforms"),
                   uiOutput("i1"),
                   p("Information panels are marked by: this icon:"),
                   imageOutput("catpaw0",height="100%", width="100%")
                   )
          ),#fluidRow
          uiOutput("i2")
)#tabPanel
