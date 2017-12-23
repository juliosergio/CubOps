library(shiny)
ui <- fluidPage(
    actionButton(inputId = "norm", label = "Normal"),
    textInput(inputId = "unif", label = "Uniform", ""),
    plotOutput("hist")
)
server <- function(input, output) {
    rv <- reactiveValues(data = rnorm(100))
    observeEvent(input$norm, { rv$data <- rnorm(100) })
    observeEvent(input$unif, { rv$data <- runif(100) })
    output$hist <- renderPlot({
        hist(rv$data)
    })
}
shinyApp(ui = ui, server = server)