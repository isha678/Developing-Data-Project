#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(datasets)

d <- mtcars
d$am <- factor(d$am, labels = c("AC", "MC"))

shinyServer(function(input, output) {
    
    formulatext <- reactive({
        paste("~ mpg", input$variable)
    })
    
    formulatext_p <- reactive({
        paste("~ mpg", "as.integer(", input$variable, ")")
    })
    
    fit <- reactive({
        lm(as.formula(formulatext_p()), data=d)
    })
    
    output$caption <- renderText({
        formulatext()
    })
    
    output$mpgBoxPlot <- renderPlot({
        boxplot(as.formula(formulatext()), 
                data = d,
                outline = input$outliers)
    })
    
    output$fit <- renderPrint({
        summary(fit())
    })
    
    output$mpgPlot <- renderPlot({
        with(d, {
            plot(as.formula(formulatext_p()))
            abline(fit(), col=2)
        })
    })
    
})