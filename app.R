#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(bslib)
library(shiny)
library(sass)
reactiveConsole(TRUE)
ui <- page_fillable(
  titlePanel("EV Total Cost of Ownership Comparison Tool"),
  title = "ICF TCO Comparison Tool",
  theme = bs_theme(preset = "litera", base_font = font_google("DM Sans", wght = "200..900")),
  layout_columns(
    card(  
           card_header("Electric Vehicle"),
           numericInput("price_elec", "Cost of electricity ($/kWh)", value = 0.2, min = 0, max = 1, step =0.01),
           numericInput("eff_EV", "Efficiency of the EV car (MPGe)", value = 100, min = 1, max = 150, step = 1),
           numericInput("cost_maint_EV", "Average EV Maintenance Cost ($/mi)", value = 0.0789, min = 0, max = 1, step = 0.01),
           numericInput("price_EV", "Price ($) of the EV you are comparing (after incentives/credits)", value = 56910, min = 0, max = 200000, step = 1000),
           ),
    card(
           card_header("Gas/Diesel Vehicle"),
           numericInput("price_gas", "Cost of fuel ($/gal)", value = 4, min = 0, max = 10, step = 0.1),
           numericInput("eff_ICE", "Efficiency of the ICE car (MPG)", value = 30, min = 1, max = 100, step = 1),
           numericInput("cost_maint_ICE", "Average ICE Maintenance Cost ($/mi)", value = 0.1013, min = 0, max = 1, step = 0.01),
           numericInput("price_ICE", "Price ($) of the ICE car you are comparing", value = 48907, min = 0, max = 200000, step = 1000),
           ),
    card(
           card_header("Both Vehicles"),
           numericInput("annual_mileage", "Annual driving mileage (mi/yr)", value = 15000, min = 0, max = 50000, step = 1000),
           input_switch("loan", "Using a loan", value = TRUE),
           conditionalPanel(
             condition = "input.loan == true",
             numericInput("borrowing_rate_percent", "Loan rate (%)", value = 7, min = 0, max = 100, step = 1),
             numericInput("loan_term", "Loan term (months)", value = 72, min = 0, max = 84, step = 1),
           ),
           conditionalPanel(
             condition = "input.loan == false",
             numericInput("loan_term", "Ownership Period (months)", value = 120, min = 0, max = 240, step = 1),
           ),
           ),
    card(
           card_header("Outputs"),
           layout_columns(
           card(
             card_header("EV Cost to Own"),
             textOutput("tco_EV"),
           ),
           card(
             card_header("ICE Car Cost to Own"),
             textOutput("tco_ICE"),
           ),
           card(
             card_header("Cost Difference"),
             textOutput("savings"),
           ),
           card(
              card_header("Equivalent MPG to Drive"), 
              textOutput("MPG_drive"),
           ),
           card(
             card_header("Equivalent MPG to Own"),
             textOutput("MPG_tco"),
           ),
           card(
             card_header("Equivalent Cost of Gas"),
             textOutput("gal"),
           ),
           col_widths = c(4,4,4,4,4,4)
           ),
           ),
    col_widths = c(4,4,4,12),
  )
)
server <- function(input, output, session) {
  eff_EV <-  reactive(input$eff_EV/33.705) #convert to mi/kWh
  cost_drive_EV <- reactive(input$price_elec/eff_EV()) #cost per mi
  cost_drive_ICE <- reactive(input$price_gas/input$eff_ICE) #cost per mi
  borrowing_rate <- reactive(input$borrowing_rate_percent/100)
  payment_EV <-  reactive({
    if (input$loan == FALSE){
      input$price_EV/input$loan_term
    } else {
      input$price_EV*borrowing_rate()*(1+borrowing_rate())^input$loan_term/((1+borrowing_rate())^input$loan_term - 1)
    }  
    })
  payment_ICE <-  reactive({
    if (input$loan == FALSE){
      input$price_ICE/input$loan_term
    } else {
      input$price_ICE*borrowing_rate()*(1+borrowing_rate())^input$loan_term/((1+borrowing_rate())^input$loan_term - 1)
    }  
  })
  cost_own_EV <- reactive(payment_EV()/input$annual_mileage) #cost per mi
  cost_own_ICE <- reactive(payment_ICE()/input$annual_mileage) #cost per mi
  
  tco_ICE <-  reactive(cost_drive_ICE() + input$cost_maint_ICE + cost_own_ICE()) #cost per mi
  tco_EV <-  reactive(cost_drive_EV() + input$cost_maint_EV + cost_own_EV()) #cost per mi
  
  output$tco_ICE <- renderText({
    paste0("The gas car costs $", toString(signif(tco_ICE(), digits = 3)), " per mile to own.")
  })
  output$tco_EV <- renderText({
    paste0("The EV costs $", toString(signif(tco_EV(), digits = 3)), " per mile to own.")
  })
  annualTCO_ICE <-  reactive(tco_ICE()*input$annual_mileage)
  annualTCO_EV <-   reactive(tco_EV()*input$annual_mileage)
  output$savings <- renderText({
    paste0("Over 5 years, the EV cost savings is $", toString(signif(5*(annualTCO_ICE() - annualTCO_EV()), digits = 3)), " in total.")
  })
  EV_equivalent_MPG <-  reactive(input$price_gas/(cost_drive_EV()+input$cost_maint_EV+cost_own_EV()-input$cost_maint_ICE-cost_own_ICE())) #mpg where TCO is equivalent, not cost to drive
  EV_equivalent_MPG_driveonly <-  reactive(input$price_gas/(cost_drive_EV())) #mpg where cost to drive is equivalent
  output$MPG_drive <- renderText({
    paste0("A gas car with ", toString(signif(EV_equivalent_MPG_driveonly(), digits = 3)), " MPG would cost the same to drive as the EV.")
  })
  output$MPG_tco <- renderText({
    paste0("A gas car with ", toString(signif(EV_equivalent_MPG(), digits = 3)), " MPG would cost the same to own as the EV.")
  })
  
  electric_gal <- reactive(input$eff_ICE*(cost_drive_EV() + input$cost_maint_EV - input$cost_maint_ICE))
  output$gal <- renderText({
    paste0("Driving the chosen EV is like paying $", toString(signif(electric_gal(), digits = 3)), " per gallon to drive your comparison ICE vehicle.")
  })
  
  }
shinyApp(ui, server)
