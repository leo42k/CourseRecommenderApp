info <- read.csv(file.path("info.csv"), stringsAsFactors = FALSE)


library(shiny)
library(DT)

shinyServer(function(input, output, session) {
    df = as.data.frame(info[,1:3])
    options(DT.options = list(pageLength = 5))
    
    # row selection

    output$x13 = DT::renderDataTable(df)
    output$y13 = renderPrint(input$x13_rows_selected)
    
    output$x14 = DT::renderDataTable(df, selection = "none")
    output$y14 = renderPrint(input$x14_rows_selected)
    
})