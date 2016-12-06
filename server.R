info <- read.csv(file.path("info.csv"), stringsAsFactors = FALSE)
info[,3] == 

library(shiny)
library(DT)

shinyServer(function(input, output, session) {
    df = as.data.frame(info[,1:3])
    options(DT.options = list(pageLength = 5))
    
    datasetInput <- reactive({
        switch(input$term,
               "1" = "1st",
               "2" = "2st",
               "3" = "3st",
               "4" = "4st",
               "S" = "Summer",
               "S1" = "Summer Inst.",
               "Int" = "Winter Inst")
    })
    
    term = datasetInput()
    
    # row selection

    df_use <- df[df[,3] == term,]
    output$x13 = DT::renderDataTable(df_use)
    output$y13 = renderPrint(input$x13_rows_selected)
    
    output$x14 = DT::renderDataTable(df, selection = "none")
    output$y14 = renderPrint(input$x14_rows_selected)
    
})