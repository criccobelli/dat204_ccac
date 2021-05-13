library(shiny)


# shiny app #
# user will type in their name and gender, and a graph with trends will be created

ui <- fluidPage(
  titlePanel("American First Name Trends"),# end title panel
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      helpText("This app will take a name and gender, and state, and provide one name trend graph for the whole country and one for the state."),
      textInput("name", "Enter a name", "Mary"),
      textInput("gender", "Enter F for female or M for male", "F"),
      textInput("state", "Enter a state abbreviation", "AK")
    ), # end sidebar panel
    mainPanel(
      plotOutput("plot"),
      plotOutput("statePlot")
    ) # end main panel
  ) # end sidebar layout
) # end fluid page


server <- function(input,output) {
  
  output$plot <- renderPlot({ # function that creates a graph when name and gender are provided
    namePlot <- names %>%
      select(firstName, birthYear, gender, total) %>%
      filter(firstName == input$name, gender == input$gender) %>%
      group_by(birthYear) %>%
      summarize(totalBabies = sum(total)) # namePlot counts all babies with a given name and gender for each year for all states
    ggplot(namePlot, aes(birthYear, totalBabies)) + geom_line(color = "blue") + 
      geom_point(size = 1) + ggtitle(paste(input$name, "- All States")) + xlab("Year") + ylab("Total Babies - Country") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0,0)) + theme_grey(base_size = 15) # axes
    # pretty breaks will show more years than the default on the x axis, the font increases in size from default,
    # expand removes the padding around the plot between the y axis and the data
  })
  
  output$statePlot <- renderPlot({ # this one only plots trend for the state provided
    namePlot <- names %>%
      select(firstName, birthYear, gender, state, total) %>% # selected state this time
      filter(firstName == input$name, gender == input$gender, state == input$state) %>% # added input$state
      group_by(birthYear) %>%
      summarize(totalState = sum(total)) # namePlot in this graph only tracks the name for the given state
    ggplot(namePlot, aes(birthYear, totalState)) + geom_line(color = "blue") + 
      geom_point(size = 1) + ggtitle(paste(input$name, "-", input$state)) + xlab("Year") + ylab("Total Babies - State") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0,0)) + theme_grey(base_size = 15)
  })
} # end server

shinyApp(ui, server) # runs app