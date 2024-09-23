# Scott Schumacker
# Loading libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "U.S. Tobacco Consumption"),
  dashboardSidebar(),
  dashboardBody(
    # KPI Row
    fluidRow(
      valueBoxOutput("TotalConsumedBox"),
      valueBoxOutput("finalConsumed"),
      valueBoxOutput("changeConsumed")
    ),
    
    # Plot Row
    fluidRow(
      box(plotOutput("populationPlot")),
      box(plotOutput("consumptionTime"))
      )
  )
)

server <- function(input, output){
  
  # Loading data
  tobaccoDF <- Adult_Tobacco_Consumption_In_The_U_S_2000_Present
  # Creating subset DF
  combustibleDF <- tobaccoDF %>% filter(Submeasure == "Total Combustible Tobacco")
  
  # Creating Key Metrics - 7 T cigarette equivalents consumed
  totalConsumption <- sum(combustibleDF$Total)
  totalConsumption <- totalConsumption/1000000000000
  totalConsumption
  
  # Total Consumed per capita in 2000
  total2000 <- subset(combustibleDF, Year == "2000", select = c("Total Per Capita"))
  firstTotal <- total2000$`Total Per Capita`
  
  # Total Consumed per capita in 2023
  total2023 <- subset(combustibleDF, Year == "2023", select = c("Total Per Capita"))
  secondTotal <- total2023$`Total Per Capita`
  
  # Calculating Percent Change
  percentChange <- round(((secondTotal - firstTotal)/firstTotal)*100,2)
  percentChange
  
  output$TotalConsumedBox <- renderValueBox({
    valueBox(
      paste0(round(totalConsumption,2), " T"), "Cigarette Evquivalents Consumed Since 2000", 
      icon = icon("list")
    )
  })
  
  output$finalConsumed <- renderValueBox({
    valueBox(
      paste0(secondTotal), "Cigarette Equivalents Consumed Per Capita in 2023", icon = icon("list")
    )
  })
  
  output$changeConsumed <- renderValueBox({
    valueBox(
      paste0(percentChange, "%"), "Tobacco Consumption from 2000 - 2023", icon = icon("list")
    )
  })
  
  output$populationPlot <- renderPlot({
    # Population Chart
    ggplot(combustibleDF, aes(Year, Population)) +
      geom_point() +
      geom_line() +
      theme_bw() +
      ggtitle("Population")
  })
  
  output$consumptionTime <- renderPlot({
    # Total Tobacco Consumption Per Capita - Cigarette Equivalents
    ggplot(combustibleDF, aes(Year, `Total Per Capita`)) +
      geom_point() +
      geom_line() +
      theme_bw() +
      ylab("Consumption Per Capita") +
      ggtitle("Total Combustible Tobacco Consumption")
  })
  
}

shinyApp(ui, server)