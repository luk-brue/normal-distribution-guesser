library(shiny)
library(ggplot2)
library(shinyFeedback)
library(shinyjs)

verteilungen <- c("Normal", "Uniform", "Bimodal")

# Define UI
ui <- fluidPage(

  # initialisiere Zusatz-Pakete
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),

    titlePanel("Verteilungen erraten"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("n", "Stichprobengröße (aka Schwierigkeitsgrad)",
                        value = 50, min = 30, max = 300, step = 1),
            # sliderInput("bins", "Anzahl Bins", min = 3, value = 20, max = 80, step = 1),
        ),

        mainPanel(
           plotOutput("plot"),
           radioButtons("radio",
                        "Aus welcher Verteilung wurde die Stichprobe gezogen?",
                        verteilungen, selected = character(0)),
           actionButton("button", "Nächster Plot"),
           tableOutput("table"),
           textOutput("text"),
           actionButton("reset", "Neustart")
        )
    )
)

# Define server logic
server <- function(input, output, session) {

  # Zufällig eine Verteilung aussuchen
  sampler <- function() sample(verteilungen, size = 1)
  # reaktiv machen
  zufall <- reactiveValues(sample = sampler())

  # initialisiere reaktive Variablen für verschiedene Zähler
  counter <- reactiveValues(richtig = 0, falsch = 0, round = 0)

  # Runden-Zähler
  observeEvent(input$button, counter$round <- counter$round + 1)

  # Momentan nur teilweise funktionierende Logik um
  # Schwierigkeitsgrad farbig darzustellen
  observeEvent(input$n, {

      feedback(inputId = "n", text = "unmöglich", color = "darkred",
               show = {input$n < 30})
      feedback(inputId = "n", text = "schwer", color = "red",
               show = {input$n > 30 && input$n <= 39})
      feedback(inputId = "n", text = "mittelschwer", color = "orange",
               show = {input$n > 39 && input$n <= 50})
      feedback(inputId = "n", text = "medium", color = "yellow",
                      show = {input$n > 50 && input$n <= 100})
      feedback(inputId = "n", text = "leicht", color = "darkgreen",
                      show = {input$n > 100 && input$n <= 200})
      feedback(inputId = "n", text = "sehr leicht", color = "lightgreen",
               show = {input$n > 200})

  })

  # Reset-Button
  observeEvent(input$reset, {
    zufall$sample <- sampler()
    counter$richtig <- 0
    counter$falsch <- 0
    counter$round <- 0
  })

  # Erfolgsquote
    output$text <- renderText({
            paste("Erfolgsquote:",
                  # Prüfe den Rundenzähler
                  if(counter$round >= 5){
                    paste(
                      round(
                        counter$richtig / (counter$richtig + counter$falsch) * 100,
                        digits = 2),
                      "%")
                    } else {"wird erst ab 5 Versuchen angezeigt"})
      })

    # Histogramm
    output$plot <- renderPlot({

      input$button # bewirkt dass der Plot neu gerendert wird durch Knopf

      # Mögliche Verteilungen definieren und eine auswählen
      x <- switch(zufall$sample,
             "Normal" = rnorm(input$n),
             "Uniform" = runif(input$n),
             "Bimodal" = c(rnorm(input$n / 2), (rnorm(input$n / 2) + 5)))

      # Sturges-Regel aber etwas angepasst, weil es zu wenig Bins für meinen Geschmack waren
      bins <- as.integer(nclass.Sturges(x) * 1.4)

      # Plot
      df <- data.frame(x)
      ggplot(df, aes(x = x)) +
        geom_histogram(bins = bins, fill = "gray", color = "black") +
        theme_minimal()
    })

    # Radio Buttons
    observeEvent(input$radio, {
                   # erfordere Auswahl bevor es weitergeht
                   req(input$radio)
                   # Bewertung des Inputs
                   test <- input$radio == zufall$sample
                   if(test){
                     counter$richtig <- counter$richtig + 1
                     showToast(type = "success",
                               message = "",
                               keepVisible = TRUE)
                   } else {
                     counter$falsch <- counter$falsch + 1
                     showToast(type = "error", message = zufall$sample,
                               keepVisible = TRUE)
                   }
                   # Verhindere weitere Auswahl durch Deaktivieren
                   disable("radio")
                 })

    # Nächster Plot-Knopf
    observeEvent(input$button, {

      # 1. Auswahlknöpfe auf "unausgewählt" zurücksetzen
      updateRadioButtons(session, inputId = "radio",
                                    selected = character(0))

                   # 2. zufällig eine der drei Verteilungen auswählen
                   zufall$sample <- sampler()

                   # 3. Reaktiviere Auswahlknöpfe
                   enable("radio")
                   hideToast(animate = F)
                 })

    # Tabelle mit richtig und falsch
    output$table <- renderTable({
      data.frame(richtig = counter$richtig, falsch = counter$falsch)
      },
    digits = 0)
}

# Run the application
shinyApp(ui = ui, server = server)
