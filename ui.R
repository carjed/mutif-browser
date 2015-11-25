
library(shiny)
library(shinyBS)
library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
library(DT)
library(RColorBrewer)

shinyUI(fluidPage(
  fluidRow(
    column(12,
           h1('Sequence Motif Mutation Browser'),
           tags$a(href="http://github.com/carjed/mutif-browser", "Instructions and Documentation"),
           fluidRow(
             column(6,
                    fluidRow(
                      column(6,
                             selectInput("category", "Select Category:",
                                         c("AT_CG" = "AT_CG",
                                           "AT_GC" = "AT_GC",
                                           "AT_TA" = "AT_TA"
                                         ),
                                         selectize = FALSE
                             )
                      ),
                      column(1,
                             checkboxGroupInput("u3", "-3",
                                                c("A" = "A",
                                                  "C" = "C",
                                                  "G" = "G",
                                                  "T" = "T"
                                                ))),
                      column(1,
                             checkboxGroupInput("u2", "-2",
                                                c("A" = "A",
                                                  "C" = "C",
                                                  "G" = "G",
                                                  "T" = "T"
                                                ))),
                      column(1,
                             checkboxGroupInput("u1", "-1",
                                                c("A" = "A",
                                                  "C" = "C",
                                                  "G" = "G",
                                                  "T" = "T"
                                                ))),
                      column(1,
                             checkboxGroupInput("d1", "+1",
                                                c("A" = "A",
                                                  "C" = "C",
                                                  "G" = "G",
                                                  "T" = "T"
                                                ))),
                      column(1,
                             checkboxGroupInput("d2", "+2",
                                                c("A" = "A",
                                                  "C" = "C",
                                                  "G" = "G",
                                                  "T" = "T"
                                                ))),
                      column(1,
                             checkboxGroupInput("d3", "+3",
                                                c("A" = "A",
                                                  "C" = "C",
                                                  "G" = "G",
                                                  "T" = "T"
                                                )))
                    ),
                  fluidRow(
                    column(12, 
                           bsCollapse(id="browse", open=NULL,
                                      bsCollapsePanel("Browse Motifs",
                                        DT::dataTableOutput("x1")
                                      )
                                    ),
                           bsCollapse(id="select", open=NULL,
                                      bsCollapsePanel("View Selected Motifs",
                                                      DT::dataTableOutput("sub")
                                      )
                           ),
                          
                           fluidRow(
                            column(6,
                                   downloadButton('downloadSubData', 'Download Selected Data')),
                            column(6,
                                   downloadButton('downloadData', 'Download Full Data'))
                           )
                    )
                  )  
             ),
             
             column(width = 6, uiOutput("plotui"), verbatimTextOutput("hover_info"))
           )
    )
  )
))
  
