library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("spacelab"),
                fluidRow(
                  column(1),
                  column(5,
                         h3("TCO Calculator: Resilient Flooring"),
                         HTML(paste("This app explores Total Cost of Ownership (TCO) calculations and comparisons between two resilient flooring products. The app is designed for customers, contractors, researchers, procurement professionals, (or anyone!) to estimate a projected costs associated with ownership and maintenance through the lifecycle of a resilient flooring product. For a complete background on this project, variable dictionary, data sources, and walkthrough of our estimation process, please view the <a href='https://rpubs.com/Averysaurus/706769'>Supplemenntal Data and Background</a> journal.<p><p>", 
                                    
                                    "This app was developed by <a href='https://www.linkedin.com/in/averysaurus/'>Avery Richards</a> in partnership, and with a great deal of guidance, from the <a href='https://ceh.org/'>Center for Environmental Health</a>. <p>" )),
                         titlePanel(""),
                         
                         
                         
                         sidebarPanel(
                           helpText("Product 1 lifespan:"),
                           numericInput(
                             inputId = "years_1",
                             label = "Years of use:",
                             value = "1",
                             min = 1,
                             width = '500%')),
                         sidebarPanel(
                           helpText("Product 2 lifespan:"),
                           numericInput(
                             inputId = "years_2",
                             label = "Years of use:",
                             value = "1",
                             min = 1,
                             width = '500%')),
                         sidebarPanel(
                           helpText("Square feet of project:"),
                           numericInput(
                             inputId = "total_ft",
                             label = "total sq. ft.",
                             value = "1",
                             min = 1,
                             width = '500%')),
                         sidebarPanel(
                           helpText("Total Cost of Ownership: product 1"),
                           textOutput("tco_num_1")),
                         sidebarPanel(
                           helpText("Total Cost of Ownership: product 2"),
                           textOutput("tco_num_2")),
                         
                  ),
                  
                  sidebarLayout(position = "left",
                                
                                sidebarPanel(width = 3,
                                             helpText("Product 1 information:"),
                                             numericInput(
                                               inputId = "price_1",
                                               label = "Purchase Price (per sq. ft.):",
                                               value = "0",
                                               min = 0,
                                               width = '70%'),
                                             numericInput(
                                               inputId = "install_1",
                                               label = "Install cost (project total):",
                                               value = "0",
                                               min = 0,
                                               width = '70%'),
                                             numericInput(
                                               inputId = "equip_1",
                                               label = " New maintenance equipment / machinery cost (total):",
                                               value = "0",
                                               min = 0,
                                               width = '70%'),
                                             numericInput(
                                               inputId = "disposal_1",
                                               label = "Disposal Cost (estimated):",
                                               value = "0",
                                               min = 0,
                                               width = '70%'),
                                             checkboxGroupInput("maint_1", "Estimated maintenance cost per year:",
                                                                choiceNames = c( 
                                                                  "Dry Buffing (weekly)", 
                                                                  "Spray Buffing (weekly)",
                                                                  "Stripping (monthly)",
                                                                  "Finishing (annually)", 
                                                                  "Sealing (annually)"),
                                                                choiceValues = c(
                                                                  0.07599119,
                                                                  0.1399745,
                                                                  0.05422632,
                                                                  0.002842789, 
                                                                  0.003205305))
                                ),
                                sidebarPanel(width = 3, 
                                             helpText("Product 2 information:"),
                                             numericInput(
                                               inputId = "price_2",
                                               label = "Purchase price (per sq. ft.):",
                                               value = "0",
                                               min = 0,
                                               width = '70%'
                                             ),
                                             numericInput(
                                               inputId = "install_2",
                                               label = "Install cost (project total):",
                                               value = "0",
                                               min = 0,
                                               width = '70%'
                                             ),
                                             numericInput(
                                               inputId = "equip_2",
                                               label = " New maintenance equipment / machinery cost (total):",
                                               value = "0",
                                               min = 0,
                                               width = '70%'),
                                             numericInput(
                                               inputId = "disposal_2",
                                               label = "Disposal Cost (estimated):",
                                               value = "0",
                                               min = 0,
                                               width = '70%'),
                                             
                                             checkboxGroupInput("maint_2", "Estimated maintenance cost per year:",
                                                                choiceNames = c( 
                                                                  "Dry Buffing (weekly)", 
                                                                  "Spray Buffing (weekly)",
                                                                  "Stripping (monthly)",
                                                                  "Finishing (annually)", 
                                                                  "Sealing (annually)"),
                                                                choiceValues = c(
                                                                  0.07599119,
                                                                  0.1399745,
                                                                  0.05422632,
                                                                  0.002842789, 
                                                                  0.003205305))
                                ),
                  )),
                
                
                plotOutput("costPlot")
)


server <- function(input, output) {
  
  
  
  # aggregrate metrics into reactive bar chart values. 
  select_price <- reactive({
    
    c(
      sum((input$price_1 * input$total_ft), input$install_1, input$equip_1, as.numeric(input$disposal_1),
          (as.numeric(input$years_1) * as.numeric(input$maint_1) * 
             input$total_ft)),
      
      sum((input$price_2 * input$total_ft), input$install_2, input$equip_2, as.numeric(input$disposal_2), 
          (as.numeric(input$years_2) * as.numeric(input$maint_2) * 
             input$total_ft)))
  })
  
  output$tco_num_1 <- renderText({
    paste('$',round(select_price()[1], 2))
  })
  
  output$tco_num_2 <- renderText({
    paste('$',round(select_price()[2], 2))
  })
  
  my_bar <- output$costPlot <- renderPlot({
    barplot(select_price(), 
            beside = T,
            border=F, 
            las=2 , 
            col=c(rgb(0.3,0.1,0.4,0.6) , 
                  rgb(0.3,0.5,0.4,0.6)) , 
            space = 3)
    
    abline(v=c(4.9 , 6.1) , col="grey")
    
    legend("top", legend = c("Product 1", "Product 2" ), 
           col = c(rgb(0.3,0.1,0.4,0.6), 
                   rgb(0.3,0.5,0.4,0.6)), 
           bty = "n",
           pch=20 , pt.cex = 4, 
           cex = 1.6, horiz = FALSE, 
           inset = c(0.05, 0.05))
    
    my_bar      
  })
}

# Run the application 
shinyApp(ui = ui, server = server)