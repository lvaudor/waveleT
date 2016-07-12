# This file is part of the WaveleT package
# Copyright 2015 2016 Lise Vaudor
# This program is distributed under the GPLv3 license

tabPanel("XWT spectrum",
         wellPanel(
           fluidRow(
             column(width=3,
                    selectInput(inputId="XWT_filter",
                                label="Filter",
                                choices=c("morlet", "paul", "dog"),
                                selected="morlet")
             ),
             column(width=3,
                    selectInput(inputId="XWT_info",
                                label="Info",
                                choices=c("wavelet",
                                          "power",
                                          "power.corr.norm",
                                          "phase",
                                          "rsq"),
                                selected="wavelet")
             ),
             column(width=2,
                   checkboxInput(inputId="XWT_plot_sig",
                                 label="Show significance",
                                 value=FALSE),
                   checkboxInput(inputId="XWT_plot.cb",
                                 label="Show colorbar",
                                 value=FALSE),
                   checkboxInput(inputId="plot.phase",
                                 label="Plot phase arrows",
                                 value=FALSE)
             ),
             column(width=2,
                    sliderInput(inputId="nrands",
                                label="nrands",
                                min=10,max=300,step=10,value=10,ticks=FALSE)
             ),
             column(width=2,
                    sliderInput(inputId="XWT_alpha",
                                label="Alpha",
                                min=0,max=1,step=0.01,value=0.05,ticks=FALSE)
             )
           )#fluidRow
         ),#wellPanel
         fluidRow(column(width=4,
                         wellPanel(
                          selectInput(inputId="XWT_plot_type",
                                       label="Plot",
                                       choices=c("1) Info=f(x,T)",
                                                 "2) Info=f(x)",
                                                 "3) Power=f(T)"),
                                       selected="1) Info=f(x,T)"),
                          conditionalPanel(
                            condition="input.XWT_plot_type=='2) Info=f(x)'",
                                     textInput(inputId="XWT_period",
                                               label="Period T",
                                               value="0"),
                                     checkboxInput(inputId="XWT_plot_maxima",
                                                   label="show local maxima",
                                                   value=FALSE),
                                     checkboxInput(inputId="XWT_plot_minima",
                                                   label="show local minima",
                                                   value=FALSE)
                          ),#conditionalPanel
                          uiOutput("slider_xlim_xwt"),
                          uiOutput("slider_ylim_xwt"),
                          textOutput("hover_xwt_x"),
                          textOutput("hover_xwt_y"),
                          downloadButton("downloadInfoXWT","Table"),
                          downloadButton("downloadFigXWT","Figure")
                         )#wellPanel
         ), #column
         column(width=8,
                plotOutput("plotXWT",hover="hover_xwt",width="100%",height="100%")
         )#column
    )#fluidRow
)#tabPanel