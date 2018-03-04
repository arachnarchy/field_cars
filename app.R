library(ggplot2)
library(scales)
library(shiny)

##---------------------------------UI------------------------------------------#

ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    sliderInput(
      inputId = "days",
      label = "Days out",
      min = 0,
      max = 90,
      value = 21
    ),
    
    numericInput(
      inputId = "rental_quote",
      label = "Quote for rental car",
      value = 762
    ),
    
    numericInput(
      inputId = "add_drivers",
      label = "Added drivers",
      value = 1
    ),
    
    sliderInput(
      inputId = "ppg",
      label = "Price per gallon",
      value = 2.3,
      min = 1.5,
      max = 5
    ),
    
    numericInput(
      inputId = "rental_mileage",
      label = "Gas mileage (rental)",
      value = 25
    ),
    
    numericInput(
      inputId = "owned_mileage",
      label = "Gas mileage (owned)",
      value = 25
    ),
    
    numericInput(
      inputId = "chg_drivers",
      label = "Daily charge per extra driver (rental)",
      value = 12
    ),
    
    numericInput(
      inputId = "reimbursement_rate",
      label = "Reimbursement rate ($/mile on owned car)",
      value = 0.535,
      step = 0.005
    ),
    
    tags$div(
      class = "header",
      checked = NA,
      tags$p("Optional parameters for owned car, if a replacement car for the
             co-owner who doesn't go on the field trip is needed, and if mile
             overage at the end of a lease should be considered.")
    ), 
    
    sliderInput(
      inputId = "replacement_quote",
      label = "Quote for replacement rental car",
      min = 0,
      max = 2000,
      value = 400
    ),
    
    numericInput(
      inputId = "extra_rate",
      label = "Excess miles charge ($/mile)",
      value = 0.2,
      step = 0.05
    )
  ),
  
  mainPanel(
    tags$div(
      class = "header",
      checked = NA,
      tags$p("This is a tool to help decide whether to rent a car for fieldwork 
             or use your own. Enter values on the left. The first plot compares
             cost to your grant for both options depending on how many miles you
             expect to drive. The second plot shows the bottom line for the car
             owner if a privately owned car is used.")
    ),
    plotOutput("lines"),
    plotOutput("take")
  )
))

##---------------------------------SERVER--------------------------------------#

server <-
  function(input, output) {
    
    rental_model <- function(miles) {
      input$rental_quote + (miles / input$rental_mileage) * input$ppg +
        (input$chg_drivers * input$add_drivers * input$days)
    }
    
    owned_model <- function(miles) {
      miles * input$reimbursement_rate
    }
    
    take_model <- function(miles) {
      # calculates whether car owner ends up with a minus or plus after the trip
      (miles * input$reimbursement_rate - (miles/input$owned_mileage) * 
        input$ppg) - input$replacement_quote - (miles * input$extra_rate)
    }

    output$lines <- renderPlot({

      cross <-
        uniroot(function(x) owned_model(x) - rental_model(x), c(1, 4000), tol = 1e-8)

      ggplot(data.frame(x = c(0, 4000)), aes(x)) +
        theme_bw(base_size = 18) +
        stat_function(fun = rental_model, geom = "line", size = 2, aes(colour = "rental")) +
        stat_function(fun = owned_model, geom = "line", size = 2, aes(colour = "owned")) +
        geom_text(
          x = cross$root + 200,
          y = rental_model(cross$root) - 100,
          label = paste0(as.character(round(cross$root), 0), " miles")
        ) +
        scale_y_continuous(label=dollar_format()) +
        theme(legend.position = "bottom") +
        ggtitle("Costs of using rental car vs. own car") +
        ylab("Cost to grant") +
        xlab("Miles put on car")
    })
    
    output$take <- renderPlot({
      ggplot(data.frame(x = c(0, 4000)), aes(x)) +
        theme_bw(base_size = 18) +
        geom_hline(yintercept = 0, colour = "red", size = 2, linetype = "dashed") +
        stat_function(fun = take_model, geom = "line", size = 2) +
        scale_y_continuous(label=dollar_format()) +
        ggtitle("Loss/gain when using own car") +
        ylab("Car owner bottom line") +
        xlab("Miles put on car")
    })
  }



shinyApp(ui = ui, server = server)



