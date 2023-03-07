library(ggplot2)
library(dplyr)
library(shiny)

# Define UI for application that draws a histogram
ui <- tagList(
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
      
      # Sidebar
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
                                      "Visa ej" = 4), selected = 4),
          br(),
          tags$b("Om standardavikelse"),
          p("Standardavvikelse är ett statistiskt mått på spridningen av data i en mängd av observationer. 
             Det kan ses som en mått på variationen i fördelningen. En hög standardavvikelse indikerar att data i fördelningen är mer spridda, 
             medan en låg standardavvikelse indikerar att data är mer samlade runt medelvärdet.", style = "font-size:13px;"),
          p("Formellt kan standardavvikelsen uttryckas som följande formel:", style = "font-size:13px;"),
          tags$img(src = "sd.gif"),
          p("där s är standardavvikelsen, x är varje enskild observation, x̄ är medelvärdet av alla observationer och n är antalet observationer.", style = "font-size:13px;"),
          p("66%, 95% och 99%-regeln är en förekommande regel inom statistiken som beskriver hur mycket av datamängden som ligger inom en viss avstånd från medelvärdet, 
                i termer av standardavvikelse. Regeln säger att ~66% av datamängden ligger inom en standardavvikelse från medelvärdet (åt respektive håll från medelvärdet), 
                ~95% av datamängden ligger inom två standardavvikelser från medelvärdet  (åt respektive håll) och ~99% av datamängden ligger inom tre standardavvikelser från medelvärdet (åt respektive håll).", 
            style = "font-size:13px;"),
          p("Dessa regler är användbara för att få en grov uppfattning om hur mycket av datamängden som ligger inom ett visst avstånd från medelvärdet, 
               och kan användas för att upptäcka eventuella utstickande observationer eller avvikelser från en förväntad fördelning. 
               Det är viktigt att notera att dessa regler endast är approximativa och att det kan finnas betydande variation från fördelning till fördelning.", style = "font-size:13px;"),
          br(),
          tags$a(
            href="https://github.com/weswasi/standardavvikelse", 
            tags$img(src="https://github.githubassets.com/images/modules/logos_page/Octocat.png",
                     width="40",
                     height="35")),
        ),
        
        # Main page
        mainPanel(
          plotOutput("plot")
        )
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
  }, height = 626)
}

# Run the application 
shinyApp(ui = ui, server = server)
