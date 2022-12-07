# Luc Watrin (2022)
# luc.watrin@uni-ulm.de

# load packages
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(plotly)
library(ggplot2)
library(markdown)

# load validity data
df_full <- read.csv2("gc_age_final_2020-08-27.csv")


# define ui
ui <- dashboardPage(
  
  # HEADER
  dashboardHeader(title = "Gc"),  
  
  # Define inputs
  dashboardSidebar(
    
    # Input: Select a file ----
    fileInput("file1", "Upload CSV File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    
    # horizontal line ----
    tags$hr(),
    
    # Input: Sex of norm sample ----
    selectInput("sex", "Sex",
                c("All" = 3,
                  "Male" = 2,
                  "Female" = 1)),
    
    # Input: Age range of norm sample ----
    sliderInput(inputId = "age_range", 
                label = "Age",
                min = 18, 
                max = 70, 
                value = c(18,70)),
    
    # Input: Sex of norm sample ----
    selectInput("edu", "Highest education level",
                c("University Degree" = 6,
                  "Vocational training" = 5,
                  "Academic track school" = 4, 
                  "Intermediate track school" = 3,
                  "Elementary or vocational track school" = 2,
                  "none" = 1)),

    # horizontal line ----
    tags$hr(),
    
    # Input: Download data ----
    downloadButton(outputId = "download_scaled_data", 
                   label = "Download Scaled Data"),
    
    # horizontal line ----
    tags$hr(),
    
    # License
    HTML(paste0("luc.watrin[at]uni-ulm.de", "<br>",
                "CC-By Attribution 4.0 International", "<br>",
                "Code available here: https://osf.io/xxx/"))
    
    
  ),
  
  
  # Define what's being output
  dashboardBody(
    
      tabsetPanel(type = "tabs",
                  tabPanel("Score Distribution", 
                           plotOutput("plot1", width = 700, height = 700)),
                  tabPanel("Debug", 
                           tableOutput("table1"))
      ))
)


# Define server logic
server <- function(input, output) { 
  
    # Data handling ----
    ## Access uploaded data set ----
    scores <- reactive({
      
              req(input$file1)
                
              read.csv(input$file1$datapath,
                                  header = TRUE,
                                  sep = ";")
    })
    
    ## Access and transform norm data set ----
    norm_data <- reactive({
      
      df_full %>% 
        # sex
        filter(sex == input$sex) %>% 
        # age range
        filter(age >= input$age_range[1] & age <= input$age_range[2]) %>% 
        # highest education
        filter(edu <= input$edu)
      
    })
  
    ## Scale uploaded data based on norm data ----
    scaled_data <- reactive({
      
      scores() %>% 
        mutate(zcg1 = (scores()$gc1 - mean(norm_data()$gc_total))/sd(norm_data()$gc_total),
               zcg2 = (scores()$gc2 - mean(norm_data()$gc_total))/sd(norm_data()$gc_total),
               zcg3 = (scores()$gc3 - mean(norm_data()$gc_total))/sd(norm_data()$gc_total))

    })
        
        
    
    # Output: Score distribution ----
    output$plot1 <- renderPlot({
              
                    if (is.null(data())) {
                      return(NULL)
                    }
                     
                    ggplot() + 
                      # density plot of norm data
                      geom_density(data = norm_data(), aes(x = gc_total*12), color = "blue") +
                      # density plot of uploaded data
                      geom_density(data = scores(), aes(x = gc1), color = "red") +
                      scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, 1))
                          
                  })
    
    # Output: Scaled data table
    output$table1 <- renderTable({
      
                     if (is.null(scaled_data())) {
                       return(NULL)
                     }
      
                     scaled_data()
                     
                    })
    
    
    # Output: Imported data plus standardized scores ----
    output$download_scaled_data <- downloadHandler(
                
                                    filename = function() {
                                      paste0('gc_scores_', Sys.Date(), ".csv")
                                      },
                                    content = function(con) {
                                      write.csv2(scaled_data(), con, row.names = FALSE)
                                      }
                                    )
    
    
    
    
      
}



shinyApp(ui, server)




