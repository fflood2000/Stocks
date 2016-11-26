library(shiny)

ui <- fluidPage(
  h1("Ex2"),
  sidebarLayout(
      sidebarPanel(
          numericInput("nrows", "Number of rows", 10)
      ),
      mainPanel(
          plotOutput("plot"),
          tableOutput("table")
      )
  )
)

server <- function(input, output, session) {
    ## Factor out the head(cars, input$nrows) so that the code isn't
    ## duplicated and the operation isn't performed twice for each
    ## change to input$nrows.

    d <- reactive({
        head(cars, input$nrows)
    })

    output$plot <- renderPlot({
        plot(d())
    })

    output$table <- renderTable({
        d()
    })
}

shinyApp(ui, server)
