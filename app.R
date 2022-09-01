#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dataMojo)
library(reactable)
library(dplyr)
## Build a survey mock data
test_dt <- data.table::data.table(
  Question = sample(c("Good", "OK", "Bad"), 90, replace = TRUE),
  Gender = c(rep("Women", 40), rep("All Others", 50)),
  Dept = c(rep("HR", 20), rep("IT", 50), rep("Finance",20)),
  Location = c(rep("Atlanta", 40), rep("New York", 20), rep("Tampa",15), rep("Beijing", 7), rep("Shanghai", 8)),
  Region = c(rep("US", 75), rep("China", 15))
)

test_dt_multi <- data.table::data.table(
  Question1 = sample(c("Good", "OK", "Bad"), 90, replace = TRUE),
  Question2 = sample(c("Good", "OK", "Bad"), 90, replace = TRUE),
  Question3 = sample(c("Good", "OK", "Bad"), 90, replace = TRUE),
  Gender = c(rep("Women", 40), rep("All Others", 50)),
  Department = c(rep("HR", 20), rep("IT", 50), rep("Finance",20)),
  Location = c(rep("Atlanta", 40), rep("New York", 20), rep("Tampa",15), rep("Beijing", 7), rep("Shanghai", 8)),
  Region = c(rep("US", 75), rep("China", 15))
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tabsetPanel(type = "tabs",
              tabPanel("Single Question Drilldown", 
                       ## build input for dynamic dimension selection
                       ## names(test_dt)[-1] is because the first col is question, which cannot be used as dimensions
                       selectizeInput("dimension_sel", 
                                      label = "Please select dimension(s):", 
                                      choices = names(test_dt)[-1],
                                      multiple = TRUE,
                                      options = list(plugins = list('remove_button', 'drag_drop'))
                       ),
                       
                       ## drilldown reactable output
                       reactableOutput("drilldown_table")
              ),
              tabPanel("Multiple questions",
                       ### Dimension selection
                       selectizeInput("dimension_sel_multi", 
                                      label = "Please select dimension(s):", 
                                      choices = names(test_dt_multi)[-c(1:3)],
                                      multiple = TRUE,
                                      options = list(plugins = list('remove_button', 'drag_drop'))
                       ),
                       selectInput("question_sel_multi", 
                                   label = "Please select survey question(s):", 
                                   choices = names(test_dt_multi)[1:3],
                                   multiple = TRUE,
                                   selected = names(test_dt_multi)[1]
                       ),
                       ## drilldown reactable output
                       reactableOutput("drilldown_table_multi")
              )
  )
  
  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$drilldown_table <- renderReactable({
    output_df <- dataMojo::pivot_percent_at(test_dt, 
                                               question_col = c("Question"), aggregated_by_cols = input$dimension_sel)|> 
      dplyr::rename_at(vars( contains("rate") ), list( ~paste(sub(".rate[0-9]value", " ", .), "(%)") ) ) |> 
      dplyr::rename_at(vars( contains("count") ), list( ~paste(sub(".count[0-9]value", " ", .), "(count)") ) ) |> 
      dplyr::rename_at(vars( contains("total") ), list( ~paste(sub(".total", " ", .), "total response") ) )
    
    reactable(
      output_df
    )
  })
  
  output$drilldown_table_multi <- renderReactable({
    
    demo_multi_output <- dataMojo::pivot_percent_at_multi(test_dt_multi, 
                                                             question_col = input$question_sel_multi, aggregated_by_cols = input$dimension_sel_multi)
    
    output_df <- demo_multi_output |> 
      dplyr::rename_at(vars( contains("rate") ), list( ~paste(sub(".rate[0-9]value", " ", .), "(%)") ) ) |> 
      dplyr::rename_at(vars( contains("count") ), list( ~paste(sub(".count[0-9]value", " ", .), "(count)") ) ) |> 
      dplyr::rename_at(vars( contains("total") ), list( ~paste(sub(".total", " ", .), "total response") ) )
    
    
    reactable(
      output_df
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


