
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidygraph)
library(reshape2)
library(stats)
library(gridExtra)
library(ggvis)

# Bring in the data
polls <- readr::read_csv('president_polls.csv')
polls$state <- as.character(polls$state)
polls$state[polls$state==''] <- 'General'
polls$state <- as.factor(polls$state)
polls$start_date <- as.Date(polls$start_date, tryFormats = "%m/%d/%y")
polls$end_date <- as.Date(polls$end_date, tryFormats = "%m/%d/%y")
Candidates <- c('Biden','Trump')
polls_TB <- subset(polls, polls$answer %in% Candidates)
same_poll <- polls_TB %>% dplyr::count(question_id)
two_count = subset(same_poll,n=2)
polls_TB <- subset(polls_TB, polls_TB$question_id %in% two_count$question_id)
polls_TB <- polls_TB %>% mutate(
    state = as.character(state),
    state = ifelse(is.na(state),"National",state),
    state = as.factor(state)
)

## Convert fte poll grade to numerical representation
polls_TB$fte_grade[is.na(polls_TB$fte_grade)] <- "F-" # convert NA to string
convert_grades <- function(x) {
    A <- factor(x, levels=c("A+", "A", "A-", "A/B",
                            "B+", "B", "B-", "B/C",
                            "C+", "C", "C-", "C/D",
                            "D+", "D", "D-", "F", "F-"))
    values <- c(1.5, 1.5, 1.5, 1.5, 
                1, 1, 1, 1,
                0.75, 0.75, 0.75, 0.75,
                0.75, 0.75, 0.5, 0.5, 0.5)
    values[A]
}

polls_TB$poll_grade <- lapply(polls_TB$fte_grade, convert_grades)


state_list <- unique(polls_TB$state)


# Define UI
ui <- fluidPage(
    # Title/description
    titlePanel("Polling Data Summary for Presidential Election 2020"),
    mainPanel(
        # Side bar description
        h3("Please enjoy my data science project on visualization of polling data"),
        h4("Select state or national polling. Size of circle indicates quality of pollster"),
        h5("Data comes from FiveThirtyEight and is updated weekly"),
        selectInput("state", "State:", 
                    choices=state_list),
    ),
    fluidRow(
        column(width = 4,
        plotOutput("plot1",
                   click = "plot1_click",
                   )
        )
        )
    )
    #fluidRow(
        #column(width = 6,
               #h4("Points near click"),
               #verbatimTextOutput("click_info")
        #)
    #)


    

## Write a function to subset and process state specific data into a dataframe for plotting

State_polling <- function(x){
    polls_x <- subset(polls_TB,polls_TB$state %in% x)
    
    polls_plot <- subset(polls_x,select = c(answer,pct,end_date,question_id,poll_grade))
    #polls_plot <- polls_plot[complete.cases(polls_plot), ]
    Biden = as.numeric()
    Trump = as.numeric()
    Question_ID = as.numeric()
    poll_grade = as.numeric()
    
    for (i in 1:length(polls_plot$question_id)){
        if (polls_plot$answer[i]=='Biden'){
            Biden = c(Biden,polls_plot$pct[i])
            Question_ID = c(Question_ID,polls_plot$question_id[i])
            poll_grade = c(poll_grade,polls_plot$poll_grade[i])
        }
        else if (polls_plot$answer[i]=='Trump'){
            Trump = c(Trump,polls_plot$pct[i])
        }
        else if (NA) {}
    }
    
    new_poll <- as.data.frame(cbind(as.numeric(Biden),as.numeric(Trump),as.numeric(Question_ID),as.numeric(poll_grade)))
    colnames(new_poll) = c('Biden','Trump','question_id',"poll_grade")
    
    new_poll <- unique(new_poll)
    
    ID_date <- subset(polls_plot, select = c('question_id','end_date'))
    ID_date <- unique(ID_date)
    new_poll <- merge(new_poll,ID_date, by = 'question_id')
    new_poll$end_date <- as.Date(new_poll$end_date, tryFormats = "%m/%d/%y")
    new_poll <- subset(new_poll, end_date > as.Date("2020-02-01")) # Just take polls starting from Feb 2020
    
    
    
    Mean_by_poll <- new_poll %>% dplyr::group_by(end_date) %>% dplyr::summarise(
        Biden = mean(Biden),
        Trump = mean(Trump), 
        Neither = 100-(Trump+Biden),
        poll_grade = mean(poll_grade)
    )
    
    return(Mean_by_poll)
    
}

## Create a function for automatically plotting the state specific data frame

df_state <- function(x) {
    state_name <- deparse(substitute(x))
    df <- subset(x, select = c(end_date,Biden,Trump,poll_grade))
    df2 <- melt(df,id.vars = c('end_date','poll_grade'), measure.vars = c('Biden','Trump'))
    return(df2)
}

# Define server logic required to draw from UI

server <- function(input, output) {
    
    # Subset data
    selected_state <- reactive({
        df_state(State_polling(input$state))
    })
    

    output$plot1 <- renderPlot({
        
        ggplot(data = selected_state(), aes(x=end_date, y = value, color = variable))+
            geom_point(aes(size = poll_grade))+
            geom_smooth(method = 'loess', aes(fill=variable),span = 5)+
            theme_bw()+
            xlab('Dates') +
            ylab('Percent Approval')+
            #ylim(30,60)+
            xlim(as.Date(c(NA,"2020-11-03")))+
            ggtitle('Trump vs Biden Polling' , paste(input$state))+
            scale_color_manual(values = c(Trump = 'red',Biden = 'blue'))+
            scale_fill_manual(values = c(Trump = 'red',Biden = 'blue'))+
            scale_size(guide=FALSE)
    })
    
    #output$click_info <- renderPrint({
    #    nearPoints(input$state, input$plot1_click, addDist = TRUE)
    #})
}


# Run the application 
shinyApp(ui = ui, server = server)
