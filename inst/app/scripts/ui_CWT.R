tabPanel("CWT spectrum",
         actionButton(inputId="i9",label=imageOutput("catpaw9",height="100%", width="100%")),
         uiOutput("i9"),br(),
         wellPanel(
           fluidRow(
             column(width=3,
                    selectInput(inputId="CWT_filter",
                                label="Filter",
                                choices=c("morlet", "paul", "dog"),
                                selected="morlet")
             ),
             column(width=3,
                    selectInput(inputId="info",
                                label="Info",
                                choices=c("wavelet",
                                          "power",
                                          "power.corr.norm"),
                                selected="wavelet")
             ),
             column(width=3,
                    checkboxInput(inputId="plot.sig",
                                  label="Show significance",
                                  value=FALSE),
                    checkboxInput(inputId="plot.cb",
                                  label="Show colorbar",
                                  value=FALSE)
                    ),
             column(width=3,
                    sliderInput(inputId="alpha",
                    label="Alpha",
                    min=0,max=1,step=0.01,value=0.05,ticks=FALSE)
             )
           )#fluidRow
         ),#wellPanel
         fluidRow(column(width=4,
                         wellPanel(
                         h3("Series y1"),
                         selectInput(inputId="CWT_plot_type1",
                                     label="Plot type",
                                     choices=c("1) Info=f(x,T)",
                                               "2) Info=f(x)",
                                               "3) Power=f(T)"),
                                     selected="1) Info=f(x,T)"),
                         uiOutput("slider_xlim_cwt1"),
                         uiOutput("slider_ylim_cwt1"),
                         conditionalPanel(
                           condition="input.CWT_plot_type1=='2) Info=f(x)'",
                           textInput(inputId="period1",
                                     label="Period T",
                                     value="0"),
                           checkboxInput(inputId="plot.maxima1",
                                         label="Show local maxima",
                                         value=FALSE),
                           checkboxInput(inputId="plot.minima1",
                                         label="Show local minima",
                                         value=FALSE)
                         ),#conditionalPanel
                         textOutput("hover_cwt1_x"),
                         textOutput("hover_cwt1_y"),
                         downloadButton("downloadInfoCWT1","Table"),
                         downloadButton("downloadFigCWT1","Figure")                         )#wellPanel
                  ), #column
                  column(width=8,
                         plotOutput("plotCWT1",hover="hover_cwt1", width="100%",height="100%")
                  )#column
         ),#fluidRow
         conditionalPanel(
           condition="(input.y2_exists==true & input.real_or_sim=='real')|
                   (input.y2sim_exists==true & input.real_or_sim=='sim')",
           hr(),
           fluidRow(column(width=4,
                           wellPanel(
                           h3("Series y2"),
                           selectInput(inputId="CWT_plot_type2",
                                       label="Plot type",
                                       choices=c("1) Info=f(x,T)",
                                                 "2) Info=f(x)",
                                                 "3) Power=f(T)"),
                                       selected="1) Info=f(x,T)"),
                           uiOutput("slider_xlim_cwt2"),
                           uiOutput("slider_ylim_cwt2"),
                           conditionalPanel(
                             condition="input.CWT_plot_type2=='2) Info=f(x)'",
                             textInput(inputId="period2",
                                        label="Period T",
                                        value="0"),
                             checkboxInput(inputId="plot.maxima2",
                                           label="Show local maxima",
                                           value=FALSE),
                             checkboxInput(inputId="plot.minima2",
                                           label="Show local minima",
                                           value=FALSE)
                           ),#conditionalPanel
                           textOutput("hover_cwt2_x"),
                           textOutput("hover_cwt2_y"),
                           downloadButton("downloadInfoCWT2","Table"),
                           downloadButton("downloadFigCWT2","Figure")
                           )#wellPanel
                   ),#column
                   column(width=8,
                          plotOutput("plotCWT2",hover="hover_cwt2", width="100%",height="100%")
                   )#column
           )#fluidRow
         )#conditionalPanel
)
