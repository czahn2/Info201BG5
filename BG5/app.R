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
      tabPanel("About", "A single page providing an overview of the project – what major questions are you seeking to answer, and what data will you use to answer those questions? This should be the first page that you see when you oppen the app. You should include some “additional flare” on this landing page, such as an image.",
      img(src = "University-of-Washington-1.jpeg")),
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
      tabPanel("Another thing",
               sidebarLayout(
                 sidebarPanel(
                   textOutput("Another widget here")
                 ),
                 mainPanel(
                   textOutput("A graph or something")
                 )
               )),
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
