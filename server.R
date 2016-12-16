info <- read.csv(file.path("info.csv"), stringsAsFactors = FALSE)

source(file.path("Yang", "code.sent.R"))

library(shiny)
library(DT)

shinyServer(function(input, output, session) {
    df = as.data.frame(info[,1:3])
    options(DT.options = list(pageLength = 5))
    
    datasetInput <- reactive({
        switch(input$term,
               "1" = "1st",
               "2" = "2nd",
               "3" = "3rd",
               "4" = "4th",
               "S" = "Summer",
               "S1" = "Summer Inst.",
               "INT" = "Winter Inst.")
    })
    
    datasetInput_term <- reactive({
        input$term
    })
    
    datasetInput_program <- reactive({
        input$program
    })
    
    output$x13 <- DT::renderDataTable({
        term <- datasetInput()
        df_use <- df[df[,3] == term,]
        df_use
    })
    output$y13 = renderPrint(input$x13_rows_selected)
    
    output$x14 = DT::renderDataTable({
        # df
        term <- datasetInput_term()
        program <- datasetInput_program()
        temp <- CourseRank(program, term)
        temp_1 <- as.matrix(temp$recommand.courses)
        temp_2 <- data.frame(course_id = temp_1)
        inner_join(temp_2, df)
    }, selection = "none")
    output$y14 = renderPrint(input$x14_rows_selected)
    
})