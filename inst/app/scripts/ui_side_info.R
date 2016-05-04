# This file is part of the WaveleT package
# Copyright 2015 2016 Lise Vaudor
# This program is distributed under the GPLv3 license

fluidPage(fluidRow(
          h3("Info"),
          h4("Scales"),
          tableOutput("tableScales"),
          h4("Coordinates"),
          tableOutput("hoverinfo")
          )
)