library(shiny)
library(ggplot2)
library(shinyFeedback)
library(shinyjs)

verteilungen <- c("Normal", "Uniform", "Bimodal")

# Define UI
ui <- fluidPage(

  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),

    # Application title
    titlePanel("Verteilungen erraten"),


    sidebarLayout(
        sidebarPanel(
            sliderInput("n", "Stichprobengröße (aka Schwierigkeitsgrad)",
                        value = 40, min = 10, max = 300, step = 1),
            # sliderInput("bins", "Anzahl Bins", min = 3, value = 20, max = 80, step = 1),
        ),

        mainPanel(
           plotOutput("plot"),
           radioButtons("radio",
                        "Aus welcher Verteilung wurde die Stichprobe gezogen?",
                        verteilungen, selected = character(0)),
           actionButton("button", "Nächster Plot"),
           tableOutput("table"),
           textOutput("text")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
sampler <- function() sample(verteilungen, size = 1)
zufall <- reactiveValues(sample = sampler())
counter <- reactiveValues(richtig = 0, falsch = 0)

    output$text <- renderText({

            paste("Erfolgsquote:",
                  if(input$button >= 5){
                    paste(
                      counter$richtig / (counter$richtig + counter$falsch) * 100,
                      "%")
                    } else {
                      "wird erst ab 5 Versuchen angezeigt"
                    }
          )

      })

    output$plot <- renderPlot({
      input$button
      x <- switch(zufall$sample,
             "Normal" = rnorm(input$n),
             "Uniform" = runif(input$n),
             "Bimodal" = c(rnorm(input$n / 2), (rnorm(input$n / 2) + 5))
      )
      df <- data.frame(x)
      bins <- as.integer(nclass.Sturges(x) * 1.4)
      ggplot(df, aes(x = x)) +
        geom_histogram(bins = bins, fill = "gray", color = "black") +
        theme_minimal()
    })
    observeEvent(input$radio,
                 {
                   req(input$radio)
                   test <- input$radio == zufall$sample
                   if(test){
                     counter$richtig <- counter$richtig + 1
                     showFeedbackSuccess(inputId = "radio", text = "richtig")
                   } else {
                     counter$falsch <- counter$falsch + 1
                   }
                   disable("radio")
                 })
    observeEvent(input$button,
                 {updateRadioButtons(session, inputId = "radio",
                                    selected = character(0))
                   zufall$sample <- sampler() # Zufallsgenerator
                   enable("radio")
                 })

    output$table <- renderTable({
      data.frame(richtig = counter$richtig, falsch = counter$falsch)
      },
    digits = 0)
}

# Run the application
shinyApp(ui = ui, server = server)
