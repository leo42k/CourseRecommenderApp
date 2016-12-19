info <- read.csv(file.path("info.csv"), stringsAsFactors = FALSE)

programs_temp <- read.csv(file.path("programs.csv"))
list_program <- programs_temp[,2]
terms_temp <- read.csv(file.path("terms.csv"))
list_term <- terms_temp[,2]


source(file.path("Yang", "code.sent.R"))
source(file.path("Zhang", "code", "time_process.R"))

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
    }, rownames= FALSE)
#    output$y13 = renderPrint(input$x13_rows_selected)
    output$y13 = DT::renderDataTable({
        term <- datasetInput()
        df_use <- df[df[,3] == term,]
        tempdf <- df_use[input$x13_rows_selected,]
        if (nrow(tempdf) >=1) {
            for (i in 1:nrow(tempdf)){
                tempdf[i,1] <- paste0('<a href=',comb[(comb[,2] == tempdf[i,1]) * (comb[,4] == term)==1, 1],'>', tempdf[i,1], '</a>')
            }
        }
        datatable(tempdf, escape = FALSE, rownames= FALSE)
    }, selection = "none")
    
    output$y15 = renderUI({
        term <- datasetInput_term()
        term_df <- datasetInput()
        df_use <- df[df[,3] == term_df,]
        list_course <- df_use[input$x13_rows_selected,1] %>% as.vector
        n <- max(length(list_course), 1)
        x <- matrix(0, n, 1)
        x[1,1] <- "Time Conflicts: "
        if (n >= 2) {
            for (i in n:2) {
                result <- time_conflict(list_course[i], term_df, list_course[1:(i-1)], term_df)
                temp1 <- paste(list_course[1:(i-1)][result], collapse = ", ")
                if (sum(result) >= 1) {
                    x[n-i+2,1] <- paste(list_course[i], "is confliced with", temp1, sep = " ")
                } else {
                    x[n-i+2,1] <- NA
                }
            }
        }
        x <- x[!is.na(x)]
        HTML(paste(x, collapse = "<br/>"))
    })
    
    output$x14 = DT::renderDataTable({
        term_df <- datasetInput()
        df_use <- df[df[,3] == term_df,]
        
        term <- datasetInput_term()
        program <- datasetInput_program()
        
        list_selected <- as.vector(df_use[input$x13_rows_selected,1])
        
        temp <- CourseRank(program, term)
        temp_1 <- as.vector(as.matrix(temp$recommand.courses))
        temp_1 <- temp_1[!(temp_1 %in% list_selected)]
        temp_2 <- data.frame(course_id = temp_1, term = rep(term_df, length(temp_1)))
        temp_3 <- inner_join(temp_2, df)[,c(1,3,2)]
        i <- which(program == list_program)
        j <- which(term == list_term)
        add_link <- function(tempdf){
            if (nrow(tempdf) >=1) {
                for (i in 1:nrow(tempdf)){
                    tempdf[i,1] <- paste0('<a href=',comb[(comb[,2] == tempdf[i,1]) * (comb[,4] == term_df)==1, 1],'>', tempdf[i,1], '</a>')
                }
            }
            return(tempdf)
        }
        if (file.exists(file.path("Yang", paste0(i, "_", j, ".rds")))) {
            rules <- readRDS(file.path("Yang", paste0(i, "_", j, ".rds")))
            Recommander <- function(input.courses){
                recommand.courses <- NULL
                rules <- (rules$rules %>% str_split(" => "))
                lhs <- lapply(rules,function(x) x[1])
                rhs <- lapply(rules,function(x) x[2])
                #covert lhs to list of vectors
                lhs <- gsub("\\{|\\}","",lhs) %>% str_split(",")
                rhs <- gsub("\\{|\\}","",rhs) %>% str_split(",")
                for (i in 1:length(lhs)){
                    if (input.courses %in% lhs[[i]]){
                        recommand.courses <- rhs[[i]]
                        break;
                    }
                }
                recommand.courses #if don't find rules, use CourseRank
            }
            list_recommendation <- sapply(list_selected, Recommander) %>% as.vector
            list_recommendation <- list_recommendation[!sapply(list_recommendation, is.null)]
            if (length(list_recommendation) >= 1) {
                temp_4 <- data.frame(course_id = list_recommendation, term = rep(term_df, length(list_recommendation)))
                temp_5 <- inner_join(temp_4, df)[,c(1,3,2)]
                temp_6 <- rbind(temp_3[(temp_3[,1] %in% temp_5[,1]),], temp_3[!(temp_3[,1] %in% temp_5[,1]),])
                temp_7 <- add_link(temp_6)
                datatable(temp_7, escape = TRUE, rownames = FALSE)
            } else {
                temp_4 <- add_link(temp_3)
                datatable(temp_4, escape = FALSE, rownames = FALSE)
            }
        } else {
            temp_4 <- add_link(temp_3)
            datatable(temp_4, escape = FALSE, rownames = FALSE)
        }
    }, selection = "none")
    output$y14 = renderPrint(input$x14_rows_selected)
    
})