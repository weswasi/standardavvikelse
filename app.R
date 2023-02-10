library(ggplot2)
library(dplyr)
library(shiny)

# Define UI for application that draws a histogram
ui <- shiny::tagList(
  withMathJax(), 
  includeCSS(path = "www/css/styles.css"), 
  
  tags$div(
    tags$div(
      class = "app_title", 
      
      titlePanel(
        title = "Standardavvikelse - Kriminologiska Institutionen", 
        windowTitle = "Standardavvikelse"
      ),
    ),
    br(),
    fluidPage(
      # Application title
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          
          sliderInput("Urvalsstorlek",
                      "Urvalsstorlek",
                      min = 50,
                      max = 550,
                      value = 300),
          sliderInput("Medelvärde",
                      "Medelvärde",
                      min = 50,
                      max = 150,
                      value = 100),
          sliderInput("Standardavvikelse",
                      "Standardavvikelse",
                      min = 1,
                      max = 29,
                      value = 15),
          
          radioButtons("std_radio", "Visa standardavikelse från medelvärdet",
                       choices = list("En standardavvikelse från medelvärdet" = 1, 
                                      "Två standardavvikelse från medelvärdet" = 2,
                                      "Tre standardavvikelse från medelvärdet" = 3,
                                      "Visa ej" = 4), selected = 4)
        ),
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("plot")
        )
      )
    ),
    tags$footer(
      tags$div(
        class = "footer_container", 
        includeHTML(path = "www/html/footer.html")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  std_data <- reactive({
    std_data <- rnorm(n = input$Urvalsstorlek,
                      mean = input$Medelvärde,
                      sd = input$Standardavvikelse)
    std_data <- data.frame(std_data) %>% 
      rename(x = std_data)
  })
  
  output$plot <- renderPlot({
    std_data() %>% 
      ggplot(aes(x = x)) +
      geom_histogram(bins = 50, color = "white", size = 0.5) +
      
      {if (input$std_radio == 1) geom_vline(xintercept = input$Medelvärde - input$Standardavvikelse, linetype = "dashed", color = "#e06666")} +
      {if (input$std_radio == 1) geom_vline(xintercept = input$Medelvärde + input$Standardavvikelse, linetype = "dashed", color = "#e06666")} +
      {if (input$std_radio == 1) geom_label(aes(x = input$Medelvärde, y = 16, label = "68 % av observationerna"), colour = "#e06666", fill = "white")} +
      {if (input$std_radio == 1) geom_segment(aes(x = input$Medelvärde - input$Standardavvikelse+1, y = 15, xend = input$Medelvärde + input$Standardavvikelse-1, yend = 15),
                                              arrow = arrow(length = unit(0.5, "cm"), ends = "both"),
                                              colour = "#e06666")} +
      
      {if (input$std_radio == 2) geom_vline(xintercept = input$Medelvärde - input$Standardavvikelse*2, linetype = "dashed", color = "#e06666")} +
      {if (input$std_radio == 2) geom_vline(xintercept = input$Medelvärde + input$Standardavvikelse*2, linetype = "dashed", color = "#e06666")} +
      {if (input$std_radio == 2) geom_label(aes(x = input$Medelvärde, y = 16, label = "95 % av observationerna"), colour = "#e06666", fill = "white")} +
      {if (input$std_radio == 2) geom_segment(aes(x = input$Medelvärde - input$Standardavvikelse*2+1, y = 15, xend = input$Medelvärde + input$Standardavvikelse*2-1, yend = 15),
                                              arrow = arrow(length = unit(0.5, "cm"), ends = "both"),
                                              colour = "#e06666")} +
      
      {if (input$std_radio == 3) geom_vline(xintercept = input$Medelvärde - input$Standardavvikelse*3, linetype = "dashed", color = "#e06666")} +
      {if (input$std_radio == 3) geom_vline(xintercept = input$Medelvärde + input$Standardavvikelse*3, linetype = "dashed", color = "#e06666")} +
      {if (input$std_radio == 3) geom_label(aes(x = input$Medelvärde, y = 16, label = "99 % av observationerna"), colour = "#e06666", fill = "white")} +
      {if (input$std_radio == 3) geom_segment(aes(x = input$Medelvärde - input$Standardavvikelse*3+1, y = 15, xend = input$Medelvärde + input$Standardavvikelse*3-1, yend = 15),
                                              arrow = arrow(length = unit(0.5, "cm"), ends = "both"),
                                              colour = "#e06666")} +
      
      scale_x_continuous(breaks = seq(0, 200, by = 10), limits = c(0, 200)) +
      theme(axis.title.x=element_blank()) +
      ylab("Antal")
  }, height = 562)
}

# Run the application 
shinyApp(ui = ui, server = server)
