library(shiny)
library(numbers)
source("main.R")

# Fill functions. All must return integer.
mod2 <- function(v, n) ifelse(v %% 2 == 0, 1, 0)
mod3 <- function(v, n) ifelse(v %% 3 == 0, 1, 0)
modn <- function(v, n) {ifelse(n == 0, 0, ifelse(v %% n == 0, 1, 0))}
prime <- function(v, n) ifelse(n == 0, 0, ifelse(isPrime(n) && modn(v, n), 1, 0))
value <- function(v, n) v
color_table <- c(
  "value mod 2 == 0" = "mod2",
  "value mod 3 == 0" = "mod3",
  "value mod n == 0" = "modn",
  "n is prime and value mod n == 0" = "prime",
  "value" = "value"
)
function_table <- c(
  "mod2" = mod2,
  "mod3" = mod3,
  "modn" = modn,
  "prime" = prime,
  "value" = value
)

desc_table <- c(
  "mod2" = "Even numbered values are colored",
  "mod3" = "Values divisible by 3 are colored",
  "modn" = "Values disivible by their row number are colored",
  "prime" = "Values divisible by their row number, in prime numbered rows are colored",
  "value" = "Color gradient according to value"
)

ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Pascal's Triangle"),
   
   sidebarLayout(
      sidebarPanel(
        selectInput('colorfn', 'color', color_table, "mod2"),
        checkboxInput('show_values', 'Show Values', TRUE),
        htmlOutput('text')
      ),
      # Show a plot of the generated distribution
      mainPanel(
        fluidRow(
          column(12, plotOutput("plot"))
        ),
        fluidRow(
          br(),
          tableOutput("references")
        )
      )
   )
))

server <- shinyServer(function(input, output) {
   output$text <- renderText({HTML(paste(
     "Each cell's value is the sum of the cells just above it",
     paste0("The triangle is arranged as n rows numbered from 0 to ", max_row),
     paste0('<br/>', desc_table[[input$colorfn]]),
     sep = '<br/>'
   ))})
   output$plot <- renderPlot({
     # choose a function
     fn <- function_table[[input$colorfn]]
     
     # apply the function to fill value
     h <- lapply(h, function(p) { p$fill <- fn(p$value, p$n); return(p)})
     p <- plot(h, input$show_values)
     print(p)
   })
   output$references <- renderTable({
     labels <- c(
       "All about Pascal's Triangle",
       "Source Code"
     )
     urls <- c(
       "https://en.wikipedia.org/wiki/Pascal%27s_triangle",
       "https://github.com/aaronferrucci/pascalr"
     )
     references <- paste0(
       labels, ": ",
       "<a href='",  urls, "' target='_blank'>",
       urls, "</a>")
     
     data.frame(references)
     
   }, sanitize.text.function = function(x) x)
})

# Run the application 
shinyApp(ui = ui, server = server)

