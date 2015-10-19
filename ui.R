library(shiny)

shinyUI(fluidPage(
        title = 'Find your University!',
        tags$style(type="text/css",
                   HTML("#mainheader h4 {margin-left: 40%;}
                        #mainheader {background-color: #E4E9F4;}")
        ),
        div(id="mainheader", h4("Find your University")),
        sidebarLayout(
                sidebarPanel(
                        width = 2,
                        h3("Enter SAT Scores"),
                        numericInput("satread", "Reading:", min = 200, max = 800, value = 500, width = "100px"),
                        numericInput("satwrite", "Writing:", min = 200, max = 800, value = 500, width = "100px"),
                        numericInput("satmath", "Math:", min = 200, max = 800, value = 500, width = "100px"),
                        radioButtons("pctrank", "Your preferred percentile rank within institution:",
                                     c("25th" = 25, "50th" = 50, "75th" = 75),
                                     selected = 50),
                        actionButton("goButton", "Find"),
                        actionButton("helpButton", "Help")
                ),
                mainPanel(
                        tabsetPanel(
                                tabPanel("Results",
                                         tags$h5(htmlOutput("result_header")),
                                         tags$p(htmlOutput("summary_info")),
                                         tags$style(type="text/css",
                                                    HTML("#results td:first-child { min-width: 200px }"),
                                                    HTML("#results td:nth-child(2) { min-width: 150px }")
                                         ),
                                         htmlOutput("results")),
                                tabPanel("Help", includeHTML("www/help.html"))),
                        tags$head(tags$script(src="util.js"))
                        )
                )))
