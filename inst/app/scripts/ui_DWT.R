fluidRow(
  actionButton(inputId="i8",label=imageOutput("catpaw8",height="100%", width="100%")),
  uiOutput("i8"),uiOutput("iDWT"),br(),
  fluidRow(
     column(width=3,
           wellPanel(
           h3("Series y1"),
           selectInput(inputId="dwt_type_1",
                       label="plot",
                       choices=c("signal decomposition","variance decomposition"),
                       selected="signal decomposition"),
           selectInput(inputId="wt_filter_1",
                       label="wavelet filter",
                       choices=c("d2","d4","d6","d8","d10","d12","d14","d16","d18",
                                 "la8","la10","la12","la14","la16","la18","la20",
                                 "bl14","bl18","bl20",
                                 "c6","c12","c18","c24","c30"),
                       selected="la8"),
           conditionalPanel(
               condition="input.dwt_type_1=='signal decomposition'",
               checkboxInput(inputId="DWT_signal_yscale1",
                             label="y-scale fixed on raw signal",
                             value=TRUE),
               checkboxInput(inputId="DWT_signal_plot1",
                             label="plot signal",
                             value=TRUE)
           ),#conditionalPanel
           uiOutput("slider_xlim_dwt1"),
           textOutput("hover_dwt1_x"),
           textOutput("hover_dwt1_y"),
           downloadButton("downloadDWT1","Table"),
           downloadButton("downloadFigDWT1","Figure")
           )#wellPanel
     ),#column
     column(width=1,
            conditionalPanel(
              condition="input.dwt_type_1=='signal decomposition'",
              hr(),h6("Levels:"),
              uiOutput("levels1")
             )#conditionalPanel
     ),#column
     column(width=8,
            plotOutput("plotDWT1",hover="hover_dwt1", width="100%",height="100%")
            )
  ),#fluidRow
  hr(),
  conditionalPanel(
        condition="(input.y2_exists==true & input.real_or_sim=='real')|
                   (input.y2sim_exists==true & input.real_or_sim=='sim')",
        fluidRow(
          column(width=3,
                wellPanel(
                h3("Series y2"),
                selectInput(inputId="dwt_type_2",
                            label="plot",
                            choices=c("signal decomposition","variance decomposition"),
                            selected="signal decomposition"),
                selectInput(inputId="wt_filter_2",
                            label="wavelet filter",
                            choices=c("d2","d4","d6","d8","d10","d12","d14","d16","d18",
                                      "la8","la10","la12","la14","la16","la18","la20",
                                      "bl14","bl18","bl20",
                                      "c6","c12","c18","c24","c30"),
                            selected="la8"),
                conditionalPanel(
                  condition="input.dwt_type_2=='signal decomposition'",
                  checkboxInput(inputId="DWT_signal_yscale2",
                                label="y-scale fixed on raw signal",
                                value=TRUE),
                  checkboxInput(inputId="DWT_signal_plot2",
                                label="plot signal",
                                value=TRUE)
                ),#conditionalPanel

                uiOutput("slider_xlim_dwt2"),
                textOutput("hover_dwt2_x"),
                textOutput("hover_dwt2_y"),
                downloadButton("downloadDWT2","Table"),
                downloadButton("downloadFigDWT2","Figure")
                )#wellPanel
         ),#column
         column(width=1,
         conditionalPanel(
           condition="input.dwt_type_2=='signal decomposition'",
           hr(),h6("Levels:"),
           uiOutput("levels2")
         )#conditionalPanel
         ),#column
         column(width=8,
                plotOutput("plotDWT2",hover="hover_dwt2", width="100%",height="100%")
         )#column
     ),#fluidRow
     hr(),
     fluidRow(
        column(width=4,
               wellPanel(
                 h3("Series y1 & y2"),
                 uiOutput("slider_xlim_dwt12"),
                 textOutput("hover_dwt_superpose_x"),
                 textOutput("hover_dwt_superpose_y"),
                 downloadButton("downloadDWT_superpose","Table"),
                 downloadButton("downloadFigDWT_superpose","Figure")
               )
               ),
        column(width=8,
               plotOutput("plotDWT_superpose",hover="hover_dwt_superpose")
               )
     )
  )#conditionalPanel
 )#fluidRow
