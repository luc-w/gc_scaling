# Luc Watrin (2022)
# luc.watrin@uni-ulm.de

# load packages
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(ggplot2)
library(tidyr)
library(dplyr)
library(markdown)

# static data sets
short <- read.csv("5256_gc_3_12_best_models.csv", sep=";")
df_full <- read.csv2("gc_age_final_2020-08-27.csv") %>% 
           # compute total scores of short scales
           mutate(gc1 = rowSums(select(., short[[1]]), na.rm = TRUE),
                  gc2 = rowSums(select(., short[[2]]), na.rm = TRUE),
                  gc3 = rowSums(select(., short[[3]]), na.rm = TRUE))
sample_data <- data.frame(id = c(1,2,3,4,5),
                          gc1 = c(5,8,9,12,11),
                          gc2 = c(6,9,8,6,10), 
                          gc3 = c(7,3,11,12,8))


# define ui
ui <- dashboardPage(
  
  # HEADER
  dashboardHeader(title = "Gc"),  
  
  # Define inputs
  dashboardSidebar(
    
    # Input: Downloadable sample data for illustration ----
    downloadButton(outputId = "download_template", 
                   label = "Download Template"),
    
    
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
                c("All" = "all",
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
                                  sep = ";") %>%
              # remove completely empty rows (if scores from < 3 parallel tests are entered)
              select(where(~ !(all(is.na(.)) | all(. == ""))))
    })
    
    ## Access and transform norm data set ----
    norm_data <- reactive({
      
      if(input$sex == "all"){
        
        df_full %>% 
        # age range
        filter(age >= input$age_range[1] & age <= input$age_range[2]) %>% 
        # highest education
        filter(edu <= input$edu)
        
      } else {
        
        df_full %>% 
        # sex
        filter(sex == input$sex) %>% 
        # age range
        filter(age >= input$age_range[1] & age <= input$age_range[2]) %>% 
        # highest education
        filter(edu <= input$edu)

      }
      

      
    })
  
    ## Scale uploaded data based on norm data ----
    scaled_data <- reactive({

      # scale uploaded scores (if available) based on defined norm sample
      scaled_data <- scores()
      scaled_data <- if("gc1" %in% colnames(scores())) mutate(scaled_data, zcg1 = (scores()$gc1 - mean(norm_data()$gc1))/sd(norm_data()$gc1))
      scaled_data <- if("gc2" %in% colnames(scores())) mutate(scaled_data, zcg2 = (scores()$gc2 - mean(norm_data()$gc2))/sd(norm_data()$gc2)) else scaled_data
      scaled_data <- if("gc3" %in% colnames(scores())) mutate(scaled_data, zcg3 = (scores()$gc3 - mean(norm_data()$gc3))/sd(norm_data()$gc3)) else scaled_data

    })

    
    # Output: Score distribution ----
    output$plot1 <- renderPlot({
              
                    if (is.null(data())) {
                      return(NULL)
                    }
                    
                    # transform data sets of uploaded socres and norm sample to long format for plotting
                    scores_long <- scores() %>% 
                                   select(any_of(c("gc1", "gc2", "gc3"))) %>% 
                                   pivot_longer(cols = everything(), names_to = "Scale", values_to = "Score") %>% 
                                   mutate(Sample = "Uploaded")
                    norm_data_long <- norm_data() %>% 
                                      select(any_of(c("gc1", "gc2", "gc3"))) %>% 
                                      pivot_longer(cols = everything(), names_to = "Scale", values_to = "Score") %>% 
                                      mutate(Sample = "Norm Sample")
                    
                    rbind(scores_long, norm_data_long) %>% 
                    ggplot(aes(x= Score, color = Sample)) + 
                      geom_density() +
                      scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, 1)) +
                      facet_grid(vars(Scale)) +
                      theme_bw()
                          
                  })
    
    # Output: Scaled data table
    output$table1 <- renderTable({
      
                     if (is.null(scaled_data())) {
                       return(NULL)
                     }
      
                     scaled_data()
                     
                    })
    
    # Output: Imported data plus standardized scores ----
    output$download_template <- downloadHandler(
                
                                    filename = "template.csv",
                                    content = function(con) {
                                      write.csv2(sample_data, con, row.names = FALSE)
                                    }
                                    
                                    )
    
    # Output: Imported data plus standardized scores ----
    output$download_scaled_data <- downloadHandler(
                
                                    filename = function() {
                                      paste0("gc_scores_", Sys.Date(), ".csv")
                                      },
                                    content = function(con) {
                                      write.csv2(scaled_data(), con, row.names = FALSE)
                                    }
                                    
                                    )
    
      
}



shinyApp(ui, server)




