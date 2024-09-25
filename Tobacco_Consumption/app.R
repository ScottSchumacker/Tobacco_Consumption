# Scott Schumacker
# app.R file for Tobacco Consumption Dashboard
# This shiny dashboard is utilizing bootstrap

# Loading libraries
library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(viridis)

# Creating cards list for UI
cards <- list(
  card(
    plotlyOutput("populationPlot")
  ))

# Creating a link object
link_github <- tags$a(shiny::icon("github"), "Github", 
href = "https://github.com/ScottSchumacker/Tobacco_Consumption", 
target = "_blank")

# User Interface
ui <- page_navbar(
  theme = bs_theme(bootswatch = "minty"),
  title = "Tobacco Consumption in the U.S.",
  sidebar = NULL,
  nav_spacer(),
  # Metrics page
  nav_panel(
    title = "Metrics",
    # KPI boxes
    layout_columns(
      fill = FALSE,
      value_box(
        title = "Total Consumption 2000-2023 (Cigarette Equivalents)",
        value = textOutput("total"),
        showcase = bs_icon("fire")
      ),
      value_box(
        title = "2023 Consumption Per Capita (Cigarette Equivalents)",
        value = textOutput("consume2023")
      ),
      value_box(
        title = "Change in Consumption 2000-2023",
        value = textOutput("change")
      )
    ),
    # Plot section
    layout_columns(cards[[1]], navset_card_underline(
      title = "Combustible Tobacco Consumption Per Capita",
      nav_panel("Consumption", plotlyOutput("consumptionTime")),
      nav_panel("Linear Regression Forecast", plotlyOutput("forecastPlot"))
    )),
    layout_columns(navset_card_underline(
      title = "Combustible Consumption Change Over Time",
      nav_panel("Percentage change", plotlyOutput("changePlot")),
      nav_panel("Rate of change", plotlyOutput("ratePlot"))
    ), navset_card_underline(
      title = "Tobacco Submeasure Comparison",
      nav_panel("Cigars", plotlyOutput("cigarPlot")),
      nav_panel("Cigarettes", plotlyOutput("cigarettePlot")),
      nav_panel("Others", plotlyOutput("otherPlot"))
    ))
  ),
  # About page
  nav_panel(
    "About",
    card(
      h1("Tobacco Consumption Dashboard"),
      p("Welcome to the Tobacco Consumption Dashboard. This dashboard allows users
      to informally explore tobacco consumption in the united states."),
      p("Creator and Maintainer: Scott Schumacker"),
      p("Dashboard version: 1.0.0"),
      p("Data last updated: July 2023"),
      p("Data Set: Adult Tobacco Consumption In The U.S., 2000-Present"),
      p("Data Source: Data.gov"),
      p("Data License:"),
      a(href = "https://opendatacommons.org/licenses/by/1-0/", "license"),
      p("Disclaimer: This dashboard is for informal exploratory purposes only. 
        This dashboard is not meant to be used for formal conclusions, publications, or formal research.")
    )
  ),
  # Links section
  nav_menu(
    title = "Links",
    nav_item(link_github), align = "right"
  )
)

# Server
server <- function(input, output){
  # Creating modal welcome popup
  showModal(modalDialog(
    title = "Welcome to the Tobacco Consumption Dashboard!",
    paste0("This dashboard is to help you explore U.S. tobacco consumption data. ",
    "Disclaimer: This dashboard is for informal exploratory purposes only.")))
  
  # Data Load and Transformation
  # Loading data
  Adult_Tobacco_Consumption_In_The_U_S_2000_Present <- read_csv("Adult_Tobacco_Consumption_In_The_U.S.__2000-Present.csv")
  tobacco_DF <- Adult_Tobacco_Consumption_In_The_U_S_2000_Present
  
  # Creating subset DF
  combustible_DF <- tobacco_DF %>% filter(Submeasure == "Total Combustible Tobacco")
  
  # Creating percent Change and rate of change columns
  combustible_DF2 <- combustible_DF %>% mutate(lagged = lag(`Total Per Capita`))
  combustible_DF2$percent_change <- 
    ((combustible_DF2$`Total Per Capita` - combustible_DF2$lagged)/combustible_DF2$lagged)*100
  combustible_DF2 <- combustible_DF2 %>% mutate(lagged_percent = lag(percent_change))
  combustible_DF2$rate_change <- 
    combustible_DF2$lagged_percent - combustible_DF2$percent_change
  
  # Creating new subset for plots
  cigarSubset <- 
    subset(tobacco_DF, Measure %in% c("Cigars") & 
             Submeasure %in% c("Small Cigars", "Large Cigars"))
  
  newSubsetDF <- subset(tobacco_DF, Measure %in% c("Loose Tobacco", 
                                                  "Smokeless Tobacco") &
                          Submeasure %in% c("Chewing Tobacco", "Pipe Tobacco", 
                                            "Roll-Your-Own Tobacco") &
                          `Data Value Unit` %in% c("Pounds"))
  
  cigaretteDF <- subset(tobacco_DF, Submeasure %in% c("Cigarette Removals"))
  
  # Creating Key Metrics - 7 T cigarette equivalents consumed
  totalConsumption <- sum(combustible_DF$Total)
  totalConsumption <- totalConsumption/1000000000000
  
  output$total <- renderText({
    paste0(round(totalConsumption,2), " T")
  })
  
  # Total Consumed per capita in 2000
  total2000 <- subset(combustible_DF, Year == "2000", 
                      select = c("Total Per Capita"))
  firstTotal <- total2000$`Total Per Capita`
  
  # Total Consumed per capita in 2023
  total2023 <- subset(combustible_DF, Year == "2023", 
                      select = c("Total Per Capita"))
  secondTotal <- total2023$`Total Per Capita`
  
  # Creating KPI output for 2023 consumed per capita
  output$consume2023 <- renderText({
    secondTotal
  })
  
  # Calculating Percent Change for key metric
  percentChange <- round(((secondTotal - firstTotal)/firstTotal)*100,2)
  percentChange
  
  # Creating KPI output for percent change from 2000-2023
  output$change <- renderText({
    paste0(round(percentChange,2), "%")
  })
  
  # Creating output for population plot
  output$populationPlot <- renderPlotly({
    ggplotly(
      ggplot(combustible_DF, aes(Year, Population)) +
        geom_point() +
        geom_line() +
        theme_bw() +
        ylab("") +
        ggtitle("Population")
    )
  })
  
  # Creating plot outputs
  # Creating output for tobacco consumption over time
  output$consumptionTime <- renderPlotly({
    ggplotly(
      ggplot(combustible_DF, aes(Year, `Total Per Capita`)) +
        geom_point() +
        geom_line() +
        theme_bw() +
        ylab("Cigarette Equivalents")
    )
  })
  
  # Creating linear regression forecast model
  tobaccoModel <- lm(`Total Per Capita` ~ Year, data = combustible_DF)
  tobaccoModel
  
  # Creating prediction data frame to use for added predicted values to plot
  Year <- c(2024,2025,2026,2027,2028,2029,2030,2031,2032,2033)
  `Total Per Capita` <- NA
  futureYearDF <- data.frame(Year, `Total Per Capita`)
  colnames(futureYearDF) <- c("Year", "Total Per Capita")
  predictionDF <- subset(combustible_DF, select = c("Year", "Total Per Capita"))
  predictionDF <- rbind(predictionDF, futureYearDF)
  predictionDF$predicted_value <- predict(tobaccoModel, predictionDF)
  
  # Creating color palette
  myColors <- c("#18BC9C", "#22577A", "#F7A072")
  
  # Forecast plot
  output$forecastPlot <- renderPlotly({
    ggplotly(
      ggplot(predictionDF, aes(Year, `Total Per Capita`)) +
        geom_point() +
        geom_line(alpha = 0.2) +
        geom_smooth(method = "lm") +
        geom_point(aes(Year, predicted_value), color = "red", alpha = 0.3) +
        theme_bw() +
        ylab("Cigarette Equivalents")
    )
  })
  
  # Creating output for tobacco consumption over time
  output$changePlot <- renderPlotly({
    ggplotly(
      ggplot(combustible_DF2, aes(Year, percent_change)) +
        geom_point(size = 3, alpha = 0.6) +
        geom_line(alpha = 0.2) + 
        geom_smooth(method = "lm", se = FALSE) +
        theme_bw() +
        ylab("Change (%)")
    )
  })
  
  # Rate of change plot
  output$ratePlot <- renderPlotly({
    ggplotly(
      ggplot(combustible_DF2, aes(Year, rate_change)) +
        geom_point(color = "#18BC9C", size = 3, alpha = 0.6) +
        geom_line(alpha = 0.2) + 
        geom_smooth(method = "lm", se = FALSE) +
        theme_bw() +
        ylab("Change (%)")
    )
  })
  
  # Cigar plot
  output$cigarPlot <- renderPlotly({
    ggplotly(
      ggplot(cigarSubset, aes(Year, `Total Per Capita`, fill = Submeasure)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = myColors) +
        theme_bw() + 
        ylab("Total Consumption Per Capita")
    )
  })
  
  # Cigarette plot
  output$cigarettePlot <- renderPlotly({
    ggplotly(
      ggplot(cigaretteDF, aes(Year, `Total Per Capita`, fill = Submeasure)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = myColors) +
        theme_bw() +
        ylab("Total Consumption Per Capita")
    )
  })
  
  # Other plot
  output$otherPlot <- renderPlotly({
    ggplotly(
      ggplot(newSubsetDF, aes(Year, `Total Per Capita`, fill = Submeasure)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = myColors) +
        theme_bw() +
        ylab("Total Consumption Per Capita")
    )
  })
}

shinyApp(ui, server)