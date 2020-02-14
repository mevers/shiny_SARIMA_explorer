library(shiny)

library(tidyverse)
library(readxl)
library(ggpubr)
library(forecast)

email <- "maurits.evers@gmail.com"
version <- "1.0"
date <- "14 February 2021"
gh <- "https://github.com/mevers/shiny_SARIMA_explorer"

ts <- "webmonthlyairportnovember2019-20200204.xls" %>%
    read_excel(sheet = 1, skip = 6) %>%
    filter(AIRPORT == "SYDNEY") %>%
    transmute(
        Airport = AIRPORT,
        Year = Year, Month = Month,
        Traffic = OUTBOUND...5) %>%
    pull(Traffic) %>%
    ts(frequency = 12, start = 2009) %>%
    log()

plot_ts <- function(ts, ylab = "", title = "") {
    autoplot(ts) +
        theme_minimal() + 
        scale_x_continuous(breaks = seq(2009, 2022)) +
        xlim(2009, 2022) +
        labs(y = ylab, title = title)
}
plot_ACF <- function(ts, subtitle = "") {
    ggAcf(ts) +
        theme_minimal() +
        labs(
            title = "Auto-correlation function",
            subtitle = subtitle) +
        scale_x_continuous(breaks = seq(0, 24, by = 2))
}
plot_PACF <- function(ts, subtitle = "") {
    ggPacf(ts) +
        theme_minimal() +
        labs(
            title = "Partial auto-correlation function",
            subtitle = subtitle) +
        scale_x_continuous(breaks = seq(0, 24, by = 2))
}


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel(htmlOutput("title_panel")),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
          actionButton("auto", "Auto SARIMA"),
          titlePanel(h3("Non-seasonal components")), 
         tags$head(
             tags$style(type="text/css", 
                        "label.control-label, .selectize-control.single { 
                        display: table-cell; 
                        text-align: left; 
                        vertical-align: middle; 
                        } 
                        label.control-label {
                        padding-right: 10px;
                        }
                        .form-group { 
                        display: table-row;
                        }
                        .selectize-control.single div.item {
                        padding-right: 15px;
                        }
                        .irs-line{
                        width: 150px;
                        }
                        .irs-grid-pol.small {
                        height: 0px;
                        }")
         ),
         sliderInput("p", "AR order p", min = 0, max = 5, value = 0),
         sliderInput("d", "Differencing order d", min = 0, max = 5, value = 0),
         sliderInput("q", "MA order q", min = 0, max = 5, value = 0),
         titlePanel(h3("Seasonal components")),
         sliderInput("P", "SAR order P", min = 0, max = 5, value = 0),
         sliderInput("D", "Differencing order D", min = 0, max = 5, value = 0),
         sliderInput("Q", "SMA order Q", min = 0, max = 5, value = 0),
         sliderInput("m", "Period", min = 0, max = 24, value = 12)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         #titlePanel(htmlOutput("title_panel")),
         plotOutput("SARIMAplots", height = "640px")
      )
   ),
   HTML(sprintf("<footer>
        Author: <a href='mailto:%s?subject=SARIMA explorer'>%s</a>,
        version %s,
        %s,
        <a href='%s'>%s</a>
        </footer>", email, email, version, date, gh, gh))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$title_panel <- renderPrint({
       HTML(sprintf(
           "SARIMA explorer: SARIMA(%i,%i,%i)(%i,%i,%i)<sub>%i</sub>",
           input$p, input$d, input$q, input$P, input$D, input$Q, input$m))
       
   })
    
   output$SARIMAplots <- renderPlot({

      fit <- arima(
          ts,
          order = c(input$p, input$d, input$q),
          seasonal = list(
              order = c(input$P, input$D, input$Q),
              period = input$m))
      model_string <- sprintf(
          "SARIMA(%i,%i,%i)(%i,%i,%i)[%i]",
          input$p, input$d, input$q, input$P, input$D, input$Q, input$m)
      gg1 <- plot_ts(
          forecast(ts, model = fit), 
          ylab = "log(y)", 
          title = "Log'ed data")
      gg2 <- plot_ts(
          fit$residuals, 
          ylab = "residuals", 
          title = sprintf("%s residuals", model_string))
      gg3 <- plot_ACF(
          fit$residuals, 
          subtitle = sprintf("%s residuals", model_string))
      gg4 <- plot_PACF(
          fit$residuals, 
          subtitle = sprintf("%s residuals", model_string))
      ggarrange(gg1, gg2, ggarrange(gg3, gg4, ncol = 2, align = "h"), nrow = 3)
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

