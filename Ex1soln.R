library(shiny)

ui <- fluidPage(
    h1("Ex1"),
    sidebarLayout(
        sidebarPanel(
            numericInput("nrows", "Number of rows", 10)
        ),
        mainPanel(
            plotOutput("plot")
        )
    )
)

server <- function(input, output, session) {
    ## Plot the first input$nrows columns of a
    ## data frame of your choosing, using head() and plot()
    output$plot <- renderPlot({
        plot(head(x = cars, n = input$nrows))
    })
}

shinyApp(ui, server)

