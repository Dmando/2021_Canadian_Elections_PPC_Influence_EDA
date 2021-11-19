#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(mapcan)

pr_geographic <- mapcan(boundaries = ridings, type = standard)
CA_No_PPC <- read_csv("../Data/CA_No_PPC.csv")
source("No_PPC.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Canada without the PPC?"),
    sidebarLayout(
      sidebarPanel(h4(strong("Determine how much of the PPC Vote do Canada's Main Parties Recieve?"), align = "center"),
                    sliderInput("con", "Conservative Party", value = 0, min = 0, max = 1),
                    sliderInput("lib", "Liberal Party", value = 0, min = 0, max = 1),
                    sliderInput("ndp", "New Democratic Party", value = 0, min = 0, max = 1),
                    sliderInput("bq", "Bloc Quebecois", value = 0, min = 0, max = 1)
               ),
      mainPanel(
        plotOutput("CanadaMap")
      )
      )
    )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  pr_geographic1 <- reactive({
    df1 <- No_PPC(input$con, input$lib, input$ndp, input$bq)
    pr_geographic <- mapcan(boundaries = ridings, type = standard)
    pr_geographic <- inner_join(pr_geographic, df1,  by = c("riding_code" = "Electoral district number"), keep = TRUE) 
    pr_geographic <- pr_geographic %>%
      select(-`riding_name_french`, -`pr_french`) %>%
      group_by(`riding_name_english`) %>%
      mutate(Winning_Party = case_when(`Winner` == "Winner" ~ `Political affiliation`)) %>%
      fill(Winning_Party, .direction = "updown") %>%
      distinct(lat, .keep_all = TRUE)
  })


  
  output$CanadaMap <- renderPlot({
    p <- ggplot(pr_geographic1(), aes(x = long, y = lat, group = group, fill = Winning_Party)) +
      geom_polygon() +
      coord_fixed() +
      theme_mapcan() +
      ggtitle("2021 Federal Electoral Results")
 
    p
  })
  
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)
