wellPanel(
  h1("Simulate Data:"),
  #######################################################
 fluidRow(
   column(width=1,
          br(),
          actionButton(inputId="i6",label=imageOutput("catpaw6",height="100%", width="100%"))
   ),
   column(width=2,
          numericInput(inputId="n",
                        label="series length",
                        value=1000,
                        min=100,max=1000000,step=1)
   ),
   column(width=2,
          checkboxInput(inputId="y2sim_exists",
                        label=HTML("Add variable y<sub>2</sub>"),
                        value=FALSE)
   )
 ),#fluidRow
 #######################################################
 uiOutput("i6"),
 #######################################################
 wellPanel(
 HTML("<h2>Signal y<sub>1</sub></h2>"),
 fluidRow(
   column(width=1,
          HTML("<h3>E<sub>1</sub></h3>")
   ),
   column(width=2,
          sliderInput(inputId = "sigma1",
                      label = HTML("Noise (&#963)"),
                      ticks=FALSE,
                      value=0,
                      min=0,max=10,step=1)
          ),
   column(width=2,
          radioButtons(inputId="noisetype1",
                       label="Noise type",
                       choices=c("white","red"),
                       selected="white")
   ),
   column(width=3,offset=4,
          plotOutput("plotNoise1", height="100px",width="200px")
   )
 ),#fluidRow
 #######################################################
 fluidRow(
     column(width=1,
            HTML("<h3> y<sub>1,a</sub></h3>")

     ),
     column(width=2,
               sliderInput(inputId = "A1a",
                           label = "A",
                           ticks=FALSE,
                           value=0,
                           min=0,max=10,step=1)
     ),
     column(width=2,
            sliderInput(inputId = "T1a",
                        label = "T",
                        ticks=FALSE,
                        value=100,
                        min=1,max=1000,step=1)
     ),
     column(width=2,
            sliderInput(inputId = "phi1a",
                        label =HTML("&#934"),
                        ticks=FALSE,
                        value=0,
                        min=0,max=1,step=0.05)
            ),
     column(width=2,
            sliderInput(inputId = "t1a",
                       label = "t",
                       ticks=FALSE,
                       value=c(1,1000),
                       min=1,max=1000,step=1)
            ),
     column(width=3,
            plotOutput("plotSimsignal1a", height="100px",width="200px")
     )
   ),#fluidRow
 ############################################################
  fluidRow(
       column(width=1,
              HTML("<h3> y<sub>1,b</sub></h3>")
       ),
       column(width=2,
              sliderInput(inputId = "A1b",
                          label = "A",
                          ticks=FALSE,
                          value=0,
                          min=0,max=10,step=1)
       ),
       column(width=2,
              sliderInput(inputId = "T1b",
                          label = "T",
                          ticks=FALSE,
                          value=100,
                          min=1,max=1000,step=1)
       ),
       column(width=2,
              sliderInput(inputId = "phi1b",
                          label = HTML("&#934"),
                          ticks=FALSE,
                          value=0,
                          min=0,max=1,step=0.05)
       ),
       column(width=2,
              sliderInput(inputId = "t1b",
                          label = "t",
                          ticks=FALSE,
                          value=c(1,1000),
                          min=1,max=1000,step=1)
       ),
       column(width=3,
              plotOutput("plotSimsignal1b", height="100px",width="200px")
       )
    ),#fluidRow
 ################################################################
       fluidRow(
         column(width=1,
                HTML("<h3> y<sub>1,c</sub></h3>")
         ),
         column(width=2,
                sliderInput(inputId = "A1c",
                            label = "A",
                            ticks=FALSE,
                            value=0,
                            min=0,max=10,step=1)
         ),
         column(width=2,
                sliderInput(inputId = "T1c",
                            label = "T",
                            ticks=FALSE,
                            value=100,
                            min=1,max=1000,step=1)
         ),
         column(width=2,
                sliderInput(inputId = "phi1c",
                            label = HTML("&#934"),
                            ticks=FALSE,
                            value=0,
                            min=0,max=1,step=0.05)
         ),
         column(width=2,
                sliderInput(inputId = "t1c",
                            label = "t",
                            ticks=FALSE,
                            value=c(1,1000),
                            min=1,max=1000,step=1)
         ),
         column(width=3,
                plotOutput("plotSimsignal1c", height="100px",width="200px")
         )
     )#fluidRow
 ),#wellPanel
 ###################################################################
 conditionalPanel(
   condition="input.y2sim_exists==true",
   wellPanel(
  HTML("<h2>Signal y<sub>2</sub></h2>"),
   fluidRow(
     column(width=1,
            HTML("<h3>E<sub>2</sub></h3>")
     ),
     column(width=2,
            sliderInput(inputId = "sigma2",
                               label = HTML("Noise (&#963)"),
                               ticks=FALSE,
                               value=0,
                               min=0,max=10,step=1)
     ),
     column(width=2,
            radioButtons(inputId="noisetype2",
                         label="Noise type",
                         choices=c("white","red"),
                         selected="white")
     ),
     column(width=3,offset=4,
            plotOutput("plotNoise2", height="100px",width="200px")
     )
   ),#fluidRow
   fluidRow(
     column(width=1,
            HTML("<h3> y<sub>2,a</sub></h3>")
     ),
     column(width=2,
            sliderInput(inputId = "A2a",
                        label = "A",
                        ticks=FALSE,
                        value=0,
                        min=0,max=10,step=1)
     ),
     column(width=2,
            sliderInput(inputId = "T2a",
                        label = "T",
                        ticks=FALSE,
                        value=100,
                        min=1,max=1000,step=1)
     ),
     column(width=2,
            sliderInput(inputId = "phi2a",
                        label = HTML("&#934"),
                        ticks=FALSE,
                        value=0,
                        min=0,max=1,step=0.05)
     ),
     column(width=2,
            sliderInput(inputId = "t2a",
                        label = "t",
                        ticks=FALSE,
                        value=c(1,1000),
                        min=1,max=1000,step=1)
     ),
     column(width=3,
            plotOutput("plotSimsignal2a", height="100px",width="200px")
     )
   ),
   fluidRow(
     column(width=1,
            HTML("<h3> y<sub>2,b</sub></h3>")
     ),
     column(width=2,
            sliderInput(inputId = "A2b",
                        label = "A",
                        ticks=FALSE,
                        value=0,
                        min=0,max=10,step=1)
     ),
     column(width=2,
            sliderInput(inputId = "T2b",
                        label = "T",
                        ticks=FALSE,
                        value=100,
                        min=1,max=1000,step=1)
     ),
     column(width=2,
            sliderInput(inputId = "phi2b",
                        label = HTML("&#934"),
                        ticks=FALSE,
                        value=0,
                        min=0,max=1,step=0.05)
     ),
     column(width=2,
            sliderInput(inputId = "t2b",
                        label = "t",
                        ticks=FALSE,
                        value=c(1,1000),
                        min=1,max=1000,step=1)
     ),
     column(width=3,
            plotOutput("plotSimsignal2b", height="100px",width="200px")
     )
   ),#fluidRow
   fluidRow(
     column(width=1,
            HTML("<h3> y<sub>2,c</sub></h3>")
     ),
     column(width=2,
            sliderInput(inputId = "A2c",
                        label = "A",
                        ticks=FALSE,
                        value=0,
                        min=0,max=10,step=1)
     ),
     column(width=2,
            sliderInput(inputId = "T2c",
                        label = "T",
                        ticks=FALSE,
                        value=100,
                        min=1,max=1000,step=1)

     ),
     column(width=2,
            sliderInput(inputId = "phi2c",
                        label = HTML("&#934"),
                        ticks=FALSE,
                        value=0,
                        min=0,max=1,step=0.05)
     ),
     column(width=2,
            sliderInput(inputId = "t2c",
                        label = "t",
                        ticks=FALSE,
                        value=c(1,1000),
                        min=1,max=1000,step=1)
     ),
     column(width=3,
            plotOutput("plotSimsignal2c", height="100px",width="200px")
     )
   )#fluidRow
   )#wellPanel
 )#conditionalPanel
)#tabPanel
