set.seed(666)

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

sample_data <- data.frame(id = 1:40,
                          gc1 = round(rnorm(40, 7, 2), 0),
                          gc2 = round(rnorm(40, 7, 2), 0),
                          gc3 = round(rnorm(40, 7, 2), 0))


# define ui
ui <- dashboardPage(
  
  # HEADER
  dashboardHeader(title = "Gc"),  
  
  # Define inputs
  dashboardSidebar(
    
    # Subtitle 1: Upload data ----
    h3("Upload", style = "margin: 20px 5px 20px 16px;"),
    
    # Input: Downloadable sample data for illustration ----
    downloadButton(outputId = "download_template", 
                   label = "Download Template",
                   style = "color: #fff; 
                            background-color: #733939; 
                            border-color: #fff;
                            padding: 5px 14px 5px 14px; 
                            margin: 5px 5px 20px 18px;"),
    
    # Input: Select a file ----
    fileInput("file1", "Upload CSV File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    
    # horizontal line ----
    tags$hr(),
    tags$hr(),
    
    # Subtitle 2: Chose reference sample  ----
    h3("Reference Sample", style = "margin: 5px 5px 20px 16px;"),

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
    selectInput("edu", "Highest Education Level",
                c("University Degree" = 6,
                  "Vocational training" = 5,
                  "Academic track school" = 4, 
                  "Intermediate track school" = 3,
                  "Elementary or vocational track school" = 2,
                  "none" = 1)),

    # horizontal line ----
    tags$hr(),
    
    # Subtitle 3: Download data ----
    h3("Download", style = "margin: 20px 5px 20px 18px;"),
    
    # Input: Download data ----
    downloadButton(outputId = "download_scaled_data", 
                   label = "Download Scaled Data",
                   style = "color: #fff; 
                            background-color: #733939; 
                            border-color: #fff;
                            width:130;
                            padding: 5px 5px 5px 5px; 
                            margin: 5px 5px 5px 16px; "),
    
    # horizontal line ----
    tags$hr(),
    
    # License
    HTML("luc.watrin[at]uni-ulm.de <br>
         CC-By Attribution 4.0 International <br>
         Code available at https://github.com/luc-w/gc_scaling")
  ),
  
  
  # Define what's being output
  dashboardBody(
    
      includeMarkdown('main.md'),
          
      tabsetPanel(type = "tabs",
                  tabPanel("Score Distribution", 
                           plotOutput("plot1", width = 700, height = 700)),
                  tabPanel("Debug", 
                           tableOutput("tab_debug"))
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
        # filter cases that match user-defined range
        filter(age >= input$age_range[1] & age <= input$age_range[2]) %>% 
        # filter cases with user-defined highest education
        filter(edu <= input$edu)
        
      } else {
        
        df_full %>% 
        # filter cases with user-defined sex
        filter(sex == input$sex) %>% 
        # filter cases that match user-defined range
        filter(age >= input$age_range[1] & age <= input$age_range[2]) %>% 
        # filter cases with user-defined highest education
        filter(edu <= input$edu)

      }
      

      
    })
  
    ## Scale uploaded data based on norm data ----
    scaled_data <- reactive({

      # scale uploaded scores (if available) based on (user-defined) norm sample
      scaled_data <- scores()
      scaled_data <- if("gc1" %in% colnames(scores())) mutate(scaled_data, zgc1 = (scores()$gc1 - mean(norm_data()$gc1))/sd(norm_data()$gc1))
      scaled_data <- if("gc2" %in% colnames(scores())) mutate(scaled_data, zgc2 = (scores()$gc2 - mean(norm_data()$gc2))/sd(norm_data()$gc2)) else scaled_data
      scaled_data <- if("gc3" %in% colnames(scores())) mutate(scaled_data, zgc3 = (scores()$gc3 - mean(norm_data()$gc3))/sd(norm_data()$gc3)) else scaled_data

    })

    
    # Output: Score distribution ----
    output$plot1 <- renderPlot({
              
                    if (is.null(data())) { # do not return a plot without data
                      return(NULL)
                    }
                    
                    # transform data sets of uploaded socres and norm sample to long format for plotting
                    scores_long <- scores() %>% 
                                   select(any_of(c("gc1", "gc2", "gc3"))) %>% 
                                   pivot_longer(cols = everything(), names_to = "Scale", values_to = "Score") %>% 
                                   mutate(Sample = "Your Data")
                    norm_data_long <- norm_data() %>% 
                                      select(any_of(c("gc1", "gc2", "gc3"))) %>% 
                                      pivot_longer(cols = everything(), names_to = "Scale", values_to = "Score") %>% 
                                      mutate(Sample = "Norm Sample")
                    
                    # figure caption
                    fig_caption <- paste0(# norm sample size
                                           "Norm sample size N = ", nrow(norm_data()), ", ",
                                           # % female
                                           round(prop.table(table(norm_data()$sex))["1"]*100, 2), "% female.")
                    
                    # Density plots: norm sample and user-uploaded data
                    rbind(scores_long, norm_data_long) %>% 
                    ggplot(aes(x= Score, fill = Sample)) + 
                      geom_density(alpha = .5) +
                      scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, 1)) +
                      #scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99")) + 
                      facet_grid(vars(Scale)) +
                      labs(caption = fig_caption) + 
                      theme_bw() +
                      theme(plot.caption = element_text(size = 14, hjust = 0, margin = margin(t = 15)))
                          
                  })
    
    
    # Output: Scaled data table
    output$tab_debug <- renderTable({
      
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




