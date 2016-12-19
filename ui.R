programs_temp <- read.csv(file.path("programs.csv"))
list_program <- programs_temp[,2]
# programs_temp <- read.csv(file.path("course","programs.csv"))
programs <- sort(as.vector(programs_temp[,2]))
# programs <- c("a", "b", "c")

terms_temp <- read.csv(file.path("terms.csv"))
list_term <- terms_temp[,2]
# terms_temp <- read.csv(file.path("course", "terms.csv"))
terms <- sort(as.vector(terms_temp[,2]))
# terms <- c("1", "2", "3", "4", "S")

rm(programs_temp, terms_temp)

library(shiny)

navbarPage(
    
    title = 'Course Recommender App',
    
    tabPanel(
        'Preferrences',
        fluidRow(
            selectInput("program", "Choose a program:", 
                        choices = programs),
            
            selectInput("term", "Choose a term:", 
                        choices = terms), 
            HTML("Please update your course plan on the Recommender Tab")
            
            
#            ,numericInput("obs", "Number of course recommendations:", 5)
        )
    ),
    
    tabPanel(
        'Recommender',
        fluidRow(
            column(
                6, h1('Planned Courses'), hr(),
                HTML("Note: please select/unselect courses in this table"),
                DT::dataTableOutput('x13')
                #            verbatimTextOutput('y13')
            ),
            column(
                6, h1('Recommended Courses'), hr(),
                DT::dataTableOutput('x14')
                #            verbatimTextOutput('y14')
            )
        ),
        fluidRow(
            h1('All Selected Courses'), 
            DT::dataTableOutput('y13'), 
            htmlOutput('y15')
        )
        
    )
)