# This file is part of the WaveleT package
# Copyright 2015 2016 Lise Vaudor
# This program is distributed under the GPLv3 license

wellPanel(h4("Graphs' characteristics"),
          fluidRow(
          column(width=4,
          sliderInput(inputId="height",
                      label="height",
                      min=200,step=100,
                      max=1200,value=400),
          sliderInput(inputId="width",
                      label="width",
                      min=200,step=100,
                      max=1600,value=700)
          ),#column
          column(width=4,
                 selectInput(inputId="graph_format",
                             label="format",
                             choices=c("png","jpeg","bmp","tiff"),
                             selected="png")
          )#column
          ),#fluidRow
          plotOutput("plotblank")
)