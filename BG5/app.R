library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)


data <- read_delim("nces330_20.csv")
hhIncome2013_21 <- read_delim("hhIncome2013_21.csv")

# ui 
ui <- fluidPage(
  
  # Application title
  titlePanel("Average Cost of College and Family Income"),
  
  
  mainPanel(
    tabsetPanel(
      tabPanel("About", h2("Importance of Information"), "This website aims to relay information about the",
               strong("average cost of college tuition and room and board"), 
               "across the United States.", h2("Main Questions"),
               "Using data, we will be answering important
               questions such as where, on average are the most and least expensive
               places to study? How is the cost of undergraduate education related to 
               poverty rates in specific states? And how does earning a college degree
               impact long term financial outcomes?", h2("Data"),"We will be using two main datasets
               to answer these questions. Our main data set was found from", strong("Kaggle"),
               "and the dataset is reliable and frequently updated with new information. 
               Our second dataset is from the US government, and contains data from
               the census. The link to our first and second datasets can be found", 
               a("here", href = "https://www.kaggle.com/datasets/kfoster150/avg-cost-of-undergrad-college-by-state?resource=download"),
               "and", a("here", href = "https://www.census.gov/data/datasets/time-series/demo/cps/cps-asec.html"), img(src = "University-of-Washington-1.jpeg", style ="width: 500px")),
      tabPanel("Plot", "This plot shows the average cost of undergraduate college across the US.
               This graph incorporates the two most costly aspects of college - tuition, along with
               room and board. You may select which cost you would like to view using
               the menu on the left.",
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput("Expense", label = "Choose expense",
                                      choices = c("Fees/Tuition", "Room/Board"),
                                      selected = c("Fees/Tuition", "Room/Board"))
                 ),
                 mainPanel(plotOutput("plot"))
               )),     
      
      tabPanel("Table", "This table shows every US state along with its average total cost
               of private and public institutions in the state throughout the last decade.
               You may use the slider on the left to select which years data you would like
               to view.",
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
      tabPanel("Household Income Map", "",
               
               sidebarLayout(
                 sidebarPanel(
                   p("Select a year to see the average household income:"),
                   selectInput("mapYear",
                               "Year",
                               unique(hhIncome2013_21$year)
                   ),
                 ),
                 mainPanel(
                   p("The map visualizes the average household income in the United States. 
                The darker purple in the map represents an average higher income, while the lighter
                represents an average lower income."),
                   plotlyOutput("plotlyMap")
                 )
               )
      ),
      tabPanel("Additional Info", "Since our data set only provides data up to 2021, these subsequent graphics show current 2023 data for Average In-State Tuition, Average Out-Of-State Tuition, and Tuition As A Percent Of Median Income for each state",
               img(src = "https://i.imgur.com/pviIcVI.jpg"), img(src = "https://i.imgur.com/KQPWqbT.jpg"), img(src = "https://i.imgur.com/QIoQie8.jpg")
      ),
      
      
      tabPanel("Conclusion", h2("Conclusion Found"), "Altogether, the", strong("average price of college has increased 
                  over the last decade"), ", as seen in the graph. Tuition and fees make up the
               majority of expenses. Despite tuition increasing, the return on investment
               in a college education also increased. It was also observed that", strong(
                 "Vermont"), "and", strong("New Hampshire"), "have two of the highest average
               costs for college. The states with the lowest average cost are", strong("Wyoming"),
               "and", strong("Utah"), ". The cost of college and household wealth varies across
               US states. As seen in the maps,", em("Southern and East Coast states"), "spend more
               from their income on the costs of college in comparison to all US states. This aligns
               the cost of in-state and out-of-state tuition, since these states have higher average
               costs of college. In relation to household income, states with higher tuition averages
               see families putting a larger percentage of their income towards their childrens'
               education. This is true for both private and public establishments. Long-term,
               these results are aimed to help simplify the financial aspect of choosing a college
               to attend. The results of our answered questions show that despite where one
               attends college, costs are rising across the US.", h2("Quality of Data Used"), "The data we used to answer our questions
               are of high quality and were gathered through proper means as they accurately 
               represent the US population. The data is assumed to give unbiased results, as the 
               sample methods used are deemed to be accurate. While no specific groups are harmed
               by the data collected, some groups may be underrepresented, and the financial backgrounds
               and situations of these households are not known. This is important to note as financial
               history and situations are often a key factor in deciding which college to attend.",
               h2("Future Advancement of This Project"),
               "To advance this project, it would be beneficial to study a sample of subjects, some that 
               obtained a college degree and some that did not, to observe how long-term income
               and career outcomes are related to earning a college degree.",
     img(src = "Conclusion_graph.JPG", style ="width: 500px") )
    )))



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
  
  mapData <- reactive({
    joe <- hhIncome2013_21 %>% 
      filter(hhIncome2013_21$year %in% input$mapYear) 
    if(nrow(joe) > 1) 
      joe <- hhIncome2013_21 %>% 
        filter(hhIncome2013_21$year == input$mapYear)
    else
      joe
  })
  
  output$plotlyMap <- renderPlotly({
    plot_ly(mapData(), 
            dataSet <- mapData(),
            dataSet$hover <- with(dataSet, paste(dataSet$state,
                                                 '<br>', "Household Income", dataSet$avgHHIncome,
                                                 '<br>' , "Poverty Rate", dataSet$povertyRate)),
            
            l <- list(color = toRGB("white"), width = 2),
            # specify some map projection/options
            graphics <- list(scope = 'usa'),
            
            map <- plot_geo(dataSet, locationmode = 'USA-states'),
            map <- map %>% add_trace(
              z = dataSet$avgHHIncome, text = dataSet$hover, locations = dataSet$abbrev,
              color = dataSet$colorsID, colors = 'Purples'
            ),
            
            map <- map %>% colorbar(title = "Thousands USD"),
            
            map <- map %>% layout(
              title = '2013-2021 Household Income in each U.S State<br>(Hover for breakdown)'
              , geo = graphics )
    )
    map
  })



}

# Run the application 
shinyApp(ui = ui, server = server)
