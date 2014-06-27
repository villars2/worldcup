library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Group Ranks"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons("gr", label = h3("Group"),
                   choices = list("Group A" = "A", 
                                  "Group B" = "B", 
                                  "Group C" = "C", 
                                  "Group D" = "D", 
                                  "Group E" = "E", 
                                  "Group F" = "F", 
                                  "Group G" = "G", 
                                  "Group H" = "H"),selected = "A")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("grouprnk")
    )
  )
))