#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Star Wars to Genesys Vehicle Converter"),
    hr(),
    em("A very simple app to help you convert Fantasy Flight Games'/Edge Studios' Star Wars Roleplaying Game vechicle profiles for use with the Genesys rules as suggested by the Order 66 Podcast"),
    br(),
    br(),
    a("Order 66 suggested conversion reference", href = "http://d20radio.com/backerzone/Order_66_Conversion_Guide-Genesys_Vehicle_Rules_in_Star_Wars.pdf"),
    hr(),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            width = 3,
            h4("Basic stats:"),
            sliderInput("sil", "Silhouette", min = 1, max = 10, value = 4),
            sliderInput("speed", "Speed", min = 1, max = 8, value = 2),
            sliderInput("handling", "Handling", min = -5, max = 5, value = 0),
            
            sliderInput("ht", "Hull Trauma", min = 0, max = 175, value = 10),
            sliderInput("ss", "System Strain", min = 0, max = 175, value = 10),

            hr(),
            h4("Armor:"),
            sliderInput("armor", "Armor", min = 0, max = 10, value = 0),
            fluidRow(
                column(4, checkboxInput("civilian", "Civilian")),
                column(4, checkboxInput("battleship", "Battleship/station")),
                column(4, radioButtons("battleship_bonus", "Battleship bonus", choices = c("+1" = 1, "+2" = 2), inline = T))
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            hr(),
            h4("Defense:"),
            uiOutput("defense_values"),
            hr(),
            br(),
            br(),
            h2("Output stats:"),
            tableOutput("profile_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$defense_values <- renderUI({
        if ( as.numeric(input$sil) <= 4) {
            fluidRow(
                column(3, numericInput("fore", "Fore", value = 1, min = 0, max = 10, step = 1)),
                column(3, numericInput("aft", "Aft", value = 1, min = 0, max = 10, step = 1))
            )
        } else {
            fluidRow(
                column(3, numericInput("fore", "Fore", value = 1, min = 0, max = 10, step = 1)),
                column(3, numericInput("aft", "Aft", value = 1, min = 0, max = 10, step = 1)),
                column(3, numericInput("left", "Left", value = 1, min = 0, max = 10, step = 1)),
                column(3, numericInput("right", "Right", value = 1, min = 0, max = 10, step = 1))
            )
        }
    })

    output$profile_table <- renderTable({
        sil <- input$sil
        speed <- min(input$speed, 5)
        handling <- input$handling
        
        def_vals <- as.numeric(c(input$fore, input$aft, input$left, input$right))
        defense <- round(mean(def_vals, na.rm = TRUE))

        civ_penalty <- (-1 * input$civilian)
        bonus <- (input$battleship * as.numeric(input$battleship_bonus)         )
        armor <- max(0, ceiling((input$armor / 2)) + bonus + civ_penalty)
        
        ht <- input$ht + input$sil
        ss <- input$ss
        
        data.frame(list(
            "Silhouette" = sil,
            "Speed" = speed,
            "Handling" = handling,
            "Defense" = defense,
            "Armor" = armor,
            "Hull Trauma" = ht,
            "System Strain" = ss
        ), stringsAsFactors = FALSE)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
