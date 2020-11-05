#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(
    navbarPage("Shiny Application",
               tabPanel("Analysis",
                        fluidPage(
                            titlePanel("This application is to indicate the relationship between variables and miles per gallon (MPG)"),
                            sidebarLayout(
                                sidebarPanel(
                                    selectInput("var", "Variable:",
                                                c("No. of Cylinders" = "cyl",
                                                  "Displacement (cu.in.)" = "disp",
                                                  "Gross horsepower" = "hp",
                                                  "Rear axle ratio" = "drat",
                                                  "Weight (lb/1000)" = "wt",
                                                  "1/4 mile time" = "qsec",
                                                  "V/S" = "vs",
                                                  "Transmission" = "am",
                                                  "Number of forward gears" = "gear",
                                                  "Number of carburetors" = "carb"
                                                )),
                                    
                                    checkboxInput("outliers", "Show BoxPlot's outliers", FALSE)
                                ),
                                
                                mainPanel(
                                    h3(textOutput("caption")),
                                    
                                    tabsetPanel(type = "tabs", 
                                                tabPanel("BoxPlot", plotOutput("mpgBoxPlot")),
                                                tabPanel("Regression model", 
                                                         plotOutput("mpgPlot"),
                                                         verbatimTextOutput("fit")
                                                )
                                    )
                                )
                            )
                        )
               ),
               tabPanel("About",
                        
                        
                        helpText("You want to find the relationship between MPG and variables"),
                        h3("Must read"),
                        p("32 observations and 11 vars"),
                        
                        a("https://class.coursera.org/regmods-008")
               ),
               tabPanel("More Data Detail",
                        h2("Road Tests"),
                        hr(),
                        h3("About"),
                        helpText("The data is taken from a reknowned automotive magazine,",
                                 " It encompasses multiple designs",
                                 " for vintage automotives."),
                        h3("Format"),
                        p("A data frame with 32 observations on 11 variables."),
                        
                        p("  [, 1]   mpg         Miles/(US) gallon"),
                        p("  [, 2]	 cyl	 Number of cylinders"),
                        p("  [, 3]	 disp	 Displacement (cu.in.)"),
                        p("  [, 4]	 hp	 Gross horsepower"),
                        p("  [, 5]	 drat	 Rear axle ratio"),
                        p("  [, 6]	 wt	 Weight (lb/1000)"),
                        p("  [, 7]	 qsec	 1/4 mile time"),
                        p("  [, 8]	 vs	 V/S"),
                        p("  [, 9]	 am	 Transmission (0 = automatic, 1 = manual)"),
                        p("  [,10]	 gear	 Number of forward gears"),
                        p("  [,11]	 carb	 Number of carburetors"),
                        
                        
               ),
               tabPanel("Revert to the repo")
               
               
    )
)

