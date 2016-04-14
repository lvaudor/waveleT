shinyUI(
navbarPage("My Wavelet Toolkat", #theme="bootstrap.css",
  tabPanel(title="INFO",
           icon=icon("info",lib="font-awesome"),
           source(findmypath("app/scripts","ui_presentation.R"))$value),
  tabPanel(title="DATA",
           icon=icon("folder",lib="font-awesome"),
           source(findmypath("app/scripts","ui_data.R"))$value
  ),
  tabPanel(title="ANALYSES",
           icon=icon("cogs",lib="font-awesome"),
             tabsetPanel(
               tabPanel(title="Fourier",
                        icon=icon("align-left",lib="glyphicon"),
                        source(findmypath("app/scripts","ui_fourier.R"))$value
               ),
               tabPanel(title="DWT",
                        icon=icon("line-chart",lib="font-awesome"),
                        source(findmypath("app/scripts","ui_DWT.R"))$value
                        ),
               tabPanel(title="CWT",
                        icon=icon("table",lib="font-awesome"),
                        source(findmypath("app/scripts","ui_CWT.R"))$value
               ),
               tabPanel(title="XWT",
                        icon=icon("table",lib="font-awesome"),
                        actionButton(inputId="i10",label=imageOutput("catpaw10",height="100%", width="100%")),
                        uiOutput("i10"),br(),
                        conditionalPanel(
                          condition="(input.y2_exists!=true & input.real_or_sim=='real')|
                                     (input.y2sim_exists!=true & input.real_or_sim=='sim')",
                          HTML("<h5> This analysis compares two signals. It is not available here because you have no signal y<sub>2</sub>.</h5>")
                        ),
                        conditionalPanel(
                            condition= "(input.y2_exists==true & input.real_or_sim=='real')|
                                        (input.y2sim_exists==true & input.real_or_sim=='sim')",
                            source(findmypath("app/scripts","ui_XWT.R"))$value
                        )
               )
             )#tabsetPanel
   ),#tabPanel
  tabPanel("OPTIONS",
           icon=icon("file-image-o",lib="font-awesome"),
           source(findmypath("app/scripts","ui_options.R"))$value)
)#navbarPage
)#shinyUI
