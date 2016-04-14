tabPanel("DATA",
         wellPanel(
          fluidRow(
             column(width=3,
                    h3("Type of data")
                    ),
             column(width=3,
                    radioButtons("real_or_sim",
                                 "",
                                 choices=c("real","sim"),
                                 selected="real")
                    ),
             column(width=6,
             p("You can analyse your own data but also
                generate data. Specifying the characteristics
                of the data to simulate will help you to understand
                how you should interpret the results of wavelet-based
                tools.")
             )
           )#fluidRow
          ),
        conditionalPanel(
          condition="input.real_or_sim=='real'",
          source(findmypath("app/scripts","ui_data_real.R"))$value
        ),
        conditionalPanel(
          condition="input.real_or_sim=='sim'",
          source(findmypath("app/scripts","ui_data_sim.R"))$value
        ),
        fluidRow(column(width=2,
                        HTML("<h3>Signal y<sub>1</sub></h3>"),
                        textOutput("hover_raw1_x"),
                        textOutput("hover_raw1_y"),
                        downloadButton("dltabRaw1","Table"),
                        downloadButton("dlfigRaw1","Figure")
        ),
        column(width=3,
               p("First lines:"),
               tableOutput("tabRaw1")
        ),
        column(width=7,
               plotOutput("plotRaw1",
                          hover="hover_raw1", width="100%",height="100%")
        )
        ),#fluidRow
        conditionalPanel(condition="(input.y2_exists==true & input.real_or_sim=='real')|
                         (input.y2sim_exists==true & input.real_or_sim=='sim')",
                         fluidRow(
                           column(width=2,
                                  HTML("<h3>Signal y<sub>2</sub></h3>"),
                                  textOutput("hover_raw2_x"),
                                  textOutput("hover_raw2_y"),
                                  downloadButton("dltabRaw2","Table"),
                                  downloadButton("dlfigRaw2","Figure")
                           ),
                           column(width=3,
                                  p("First lines:"),
                                  tableOutput("tabRaw2")
                           ),
                           column(width=7,
                                  plotOutput("plotRaw2",
                                             hover="hover_raw2",width="100%",height="100%")
                           )
                         )#fluidRow
        )#conditionalPanel
)

