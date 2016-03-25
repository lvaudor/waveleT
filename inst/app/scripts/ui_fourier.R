wellPanel(
         actionButton(inputId="i7",label=imageOutput("catpaw7",height="100%", width="100%")),
         uiOutput("i7"),
         fluidRow(
           column(width=4,
                  h3("Series y1"),
                  textOutput("hover_fourier1_x"),
                  textOutput("hover_fourier1_y"),
                  downloadButton("dltabFourier1","Table"),
                  downloadButton("dlfigFourier1","Figure")
           ),
           column(width=8,
                  plotOutput("plotFourier1",
                             hover="hover_fourier1", width="100%",height="100%")
           )
         ),#fluidRow
         conditionalPanel(
           condition="(input.y2_exists==true & input.real_or_sim=='real')|
                      (input.y2sim_exists==true & input.real_or_sim=='sim')",
           fluidRow(
             column(width=4,
                    h3("Series y2"),
                    textOutput("hover_fourier2_x"),
                    textOutput("hover_fourier2_y"),
                    downloadButton("dltabFourier2","Table"),
                    downloadButton("dlfigFourier2","Figure")
             ),
             column(width=8,
                    plotOutput("plotFourier2",
                               hover="hover_fourier2", width="100%",height="100%")
             )
           )#fluidRow
         )#conditionalPanel
)
