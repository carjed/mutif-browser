
library(shiny)
library(shinyBS)
library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
library(DT)
library(RColorBrewer)
library(shinyjs)
library(shinythemes)

shinyUI(
  fluidPage(

    theme = shinytheme("flatly"),
    shinyjs::useShinyjs(),

    h1('Sequence Motif Mutation Browser'),
    # tags$a(href="http://github.com/carjed/mutif-browser", "Instructions and Documentation"),

    navbarPage("",
      tabPanel("App",
        sidebarLayout(
          sidebarPanel(
            fluidRow(
              column(6,
               selectInput("category", "Select mutation type:",
                 c("A>C (T>G)" = "AT_CG",
                   "A>G (T>C)" = "AT_GC",
                   "A>T (T>A)" = "AT_TA",
                   "C>T (G>A)" = "GC_AT",
                   "C>G (G>C)" = "GC_CG",
                   "C>A (G>T)" = "GC_TA"), selectize = FALSE)
              )
            ),

            fluidRow(
                column(1,
                  checkboxGroupInput("u3", "-3",
                    c("A" = "A",
                      "C" = "C",
                      "G" = "G",
                      "T" = "T"))),
                column(1,
                  checkboxGroupInput("u2", "-2",
                    c("A" = "A",
                      "C" = "C",
                      "G" = "G",
                      "T" = "T"))),
                column(1,
                  checkboxGroupInput("u1", "-1",
                    c("A" = "A",
                      "C" = "C",
                      "G" = "G",
                      "T" = "T"))),
                column(1,
                  checkboxGroupInput("d1", "+1",
                    c("A" = "A",
                      "C" = "C",
                      "G" = "G",
                      "T" = "T"))),
                column(1,
                  checkboxGroupInput("d2", "+2",
                    c("A" = "A",
                      "C" = "C",
                      "G" = "G",
                      "T" = "T"))),
                column(1,
                  checkboxGroupInput("d3", "+3",
                    c("A" = "A",
                      "C" = "C",
                      "G" = "G",
                      "T" = "T")))
              )
          ),

          mainPanel(
            fluidRow(
              column(12, uiOutput("plotui"), verbatimTextOutput("hover_info"))
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
                    downloadButton('downloadSubData', 'Download Selected Data')
                  ),
                  column(6,
                    downloadButton('downloadData', 'Download Full Data')
                  )
                )
              )
            )
          )

        )
      ),

      tabPanel("Documentation",
        includeMarkdown("README.md")
      )
    )
  )
)
