# Scott Schumacker
# app.R file for Tobacco Consumption Dashboard

# Loading libraries
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)

# User Interface
ui <- page_sidebar(
  title = "U.S. Tobacco Consumption",
  sidebar = sidebar(
    title = "Navigation"
  ),
  layout_columns(cards[[1]], cards[[2]]),
  layout_columns(cards[[3]], cards[[4]])
)

# Server
server <- function(input, output){
  # Loading data
  tobaccoDF <- Adult_Tobacco_Consumption_In_The_U_S_2000_Present
  # Creating subset DF
  combustibleDF <- tobaccoDF %>% filter(Submeasure == "Total Combustible Tobacco")
  
  # Creating percent Change and rate of change columns
  combustibleDF2 <- combustibleDF %>% mutate(lagged = lag(`Total Per Capita`))
  combustibleDF2$percent_change <- 
    ((combustibleDF2$`Total Per Capita` - combustibleDF2$lagged)/combustibleDF2$lagged)*100
  combustibleDF2 <- combustibleDF2 %>% mutate(lagged_percent = lag(percent_change))
  combustibleDF2$rate_change <- 
    combustibleDF2$lagged_percent - combustibleDF2$percent_change
  
  # Creating chewing tobacco subset
  cigarSubset <- 
    subset(tobaccoDF, Measure %in% c("Cigars") & 
             Submeasure %in% c("Small Cigars", "Large Cigars"))
  
  # Creating Key Metrics - 7 T cigarette equivalents consumed
  totalConsumption <- sum(combustibleDF$Total)
  totalConsumption <- totalConsumption/1000000000000
  
  # Total Consumed per capita in 2000
  total2000 <- subset(combustibleDF, Year == "2000", select = c("Total Per Capita"))
  firstTotal <- total2000$`Total Per Capita`
  
  # Total Consumed per capita in 2023
  total2023 <- subset(combustibleDF, Year == "2023", select = c("Total Per Capita"))
  secondTotal <- total2023$`Total Per Capita`
  
  # Calculating Percent Change for key metric
  percentChange <- round(((secondTotal - firstTotal)/firstTotal)*100,2)
  percentChange
  
  # Creating output for total tobacco consumed metric
  
  # Creating output for tobacco consumed in 2023 per capita metric
  
  # Creating output for tobacco consumption change
  
  # Creating cards list
  cards <- list(
    card(
      plotlyOutput("populationPlot")
    ),
    card(
      plotlyOutput("consumptionTime")
    ),
    card(
      plotlyOutput("changePlot")
    ),
    card(
      plotlyOutput("cigarPlot")
    )
  )
  
  # Creating output for population plot
  output$populationPlot <- renderPlotly({
    ggplotly(
      ggplot(combustibleDF, aes(Year, Population)) +
        geom_point() +
        geom_line() +
        theme_bw() +
        ggtitle("Population")
    )
  })
  
  # Creating output for tobacco consumption over time
  output$consumptionTime <- renderPlotly({
    ggplotly(
      ggplot(combustibleDF, aes(Year, `Total Per Capita`)) +
        geom_point() +
        geom_line() +
        theme_bw() +
        ylab("Consumption Per Capita (Cigarette Equivalents)") +
        ggtitle("Combustible Tobacco Consumption Over Time")
    )
  })
  
  # Creating output for tobacco consumption over time
  output$changePlot <- renderPlotly({
    ggplotly(
      ggplot(combustibleDF2, aes(Year, percent_change)) +
        geom_point() +
        geom_point(aes(Year, rate_change), color = "red") +
        geom_line() +
        geom_line(aes(Year, rate_change), color = "red") +
        theme_bw() +
        ylab("Change (%)") +
        ggtitle("Consumption Change Over Time") 
    )
  })
  
  # Cigar plot
  output$cigarPlot <- renderPlotly({
    ggplotly(
      ggplot(cigarSubset, aes(Year, `Total Per Capita`, fill = Submeasure)) +
        geom_bar(stat = "identity")
    )
  })
  
}

shinyApp(ui, server)