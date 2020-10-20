#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(usmap)
load("./04_rasp.Rdata")

# Define UI for application
ui <- fluidPage(
  
  # App title ----
  titlePanel("Data Analysis Visulization"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select the year ----
      selectInput("year",
                  "Select Year:",
                  choices = c(2015:2019),
                  selected = 2015)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("ACRES HARVESTED", tableOutput("table1"), plotOutput("plot1")),
                  tabPanel("PRODUCTION", tableOutput("table2"), plotOutput("plot2")),
                  tabPanel("YIELD", tableOutput("table3"), plotOutput("plot3")),
                  tabPanel("MAP of YIELD", plotOutput("plot4"))
      )
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$table1 = renderTable({
     dat2 %>%
       filter(Year == input$year) %>%
       select(Year, State, ACRES_HARVESTED) %>%
       mutate(Year = as.integer(Year))
   })
   
   output$table2 = renderTable({
     dat2 %>%
       filter(Year == input$year) %>%
       select(Year, State, PRODUCTION) %>%
       mutate(Year = as.integer(Year))
   })
   
   output$table3 = renderTable({
     dat2 %>%
       filter(Year == input$year) %>%
       select(Year, State, YIELD) %>%
       mutate(Year = as.integer(Year))
   })
   
   output$plot1 = renderPlot({
     dat2 %>% 
       ggplot(aes(x = Year, y = ACRES_HARVESTED, color = State)) +
       geom_line() +
       labs(title = "Area of Harvested of the last 5 years")
   })
   
   output$plot2 = renderPlot({
     dat2 %>% 
       ggplot(aes(x = Year, y = PRODUCTION, color = State)) +
       geom_line() +
       labs(title = "PRODUCTION (in lbs) of the last 5 years")
   })
   
   output$plot3 = renderPlot({
     dat2 %>% 
       ggplot(aes(x = Year, y = YIELD, color = State)) +
       geom_line() +
       labs(title = "YIELD (in lbs per Acre) of the last 5 years")
   })
   
   output$plot4 = renderPlot({
     
     p4 = dat2 %>%
       filter(Year == input$year) %>%
       select(State, YIELD) %>%
       mutate(State = str_to_title(State)) %>%
       right_join(statepop, by = c("State" = "full"))
     
     plot_usmap(data = p4, regions = "states", values = "YIELD", color = "white") + 
       scale_fill_continuous(name = "YIELD", low = "yellow", high = "red") +
       theme(legend.position = "right")
     
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

