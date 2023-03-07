library(shiny)
library(tidyverse)
library(ggplot2)


data <- read_delim("nces330_20.csv")

# ui 
ui <- fluidPage(
  
  # Application title
  titlePanel("Average Cost of College and Family Income"),
  
  
  mainPanel(
    tabsetPanel(
      tabPanel("About", "This website aims to relay information about the",
               strong("average cost of college tuition and room and board"), 
               "across the United States. Using data, we will be answering important
               questions such as where, on average are the most and least expensive
               places to study? How is the cost of undergraduate education related to 
               poverty rates in specific states? And how does earning a college degree
               impact long term financial outcomes? We will be using two main datasets
               to answer these questions. Our main data set was found from", strong("Kaggle"),
               "and the dataset is reliable and frequently updated with new information. 
               Our second dataset is from the US government, and contains data from
               the census. The link to our first and second datasets can be found", 
               a("here", href = "https://www.kaggle.com/datasets/kfoster150/avg-cost-of-undergrad-college-by-state?resource=download"),
               "and", a("here", href = "https://www.census.gov/data/datasets/time-series/demo/cps/cps-asec.html")),
      tabPanel("Plot", "text about plot",
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput("Expense", label = "Choose expense",
                                      choices = c("Fees/Tuition", "Room/Board"),
                                      selected = c("Fees/Tuition", "Room/Board"))
                 ),
                 mainPanel(plotOutput("plot"))
               )),     
      
      tabPanel("Table", "text about table",
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("year_range", label = "Choose the year range",
                               min = min(data$Year),
                               max = max(data$Year),
                               value = c(2013, 2021)),
                   textOutput("year_text")
                 ),
                 mainPanel(
                   dataTableOutput("dataTable")
                 )
               )),
      tabPanel("Erik's dataset plot",
               sidebarLayout(
                 sidebarPanel(
                   textOutput("Another widget here")
                 ),
                 mainPanel(
                   textOutput("A graph or something")
                 )
               )),
      tabPanel("Additional Info", "Since our data set only provides data up to 2021, these subsequent graphics show current 2023 data for Average In-State Tuition, Average Out-Of-State Tuition, and Tuition As A Percent Of Median Income for each state",
               img(src = "https://i.imgur.com/pviIcVI.jpg"), img(src = "https://i.imgur.com/KQPWqbT.jpg"), img(src = "https://i.imgur.com/QIoQie8.jpg")
      ),
      
      
      tabPanel("Conclusion", "Summary takeaways, a page that hones in on at least 2 major takeaways from the project, at least one related to your analysis, and another to the dataset. Feel free to incorporate tables, graphics, or other elements to convey these conclusions."
      )
    ))
)


# server logic!
server <- function(input, output) {
  
  output$dataTable <- renderDataTable({
    data %>%
      filter(Year >= input$year_range[1],
             Year <= input$year_range[2])
  })
  
  
  output$plot <- renderPlot({
    data %>%
      filter(Expense %in% input$Expense) %>%
      group_by(Expense) %>%
      ggplot(aes(Year, Value, group = Expense, col = factor(Expense))) +
      ggtitle("Average Cost of Undergrad College")+
      geom_line() +
      geom_point()
  })
  output$color_text <- renderText({
    paste("You chose: ", input$color)
  })
  output$year_text <- renderText({
    paste("You chose:", input$year_range[1],"-", input$year_range[2])
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
