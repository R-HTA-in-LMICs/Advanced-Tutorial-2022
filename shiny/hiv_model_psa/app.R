# Misc settings -----------------------------------------------------------
pkgs <- c("dampack", "reshape2", "tidyverse", "darthtools", "shiny", "shinyWidgets")

# Install packages if not installed
installed_packages <- pkgs %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
 install.packages(pkgs[!installed_packages])
}

# Load required packages
invisible(lapply(pkgs, library, character.only = TRUE))

# run economic analysis using the code found in the wrapper function:
source("wrapper.R")

# User interface ----------------------------------------------------------
# Create user interface using fluidpage function
ui <- fluidPage(
 # Set theme
 theme = bslib::bs_theme(bootswatch = "simplex"),
 # Create title panel with company logo
 titlePanel(title = div(img(src = "avatar.png", height = 60, align = "center"), # add logo
                        "A Case Study in HIV", # title
                        style = "color: #3765B9"), # text colour
            windowTitle = "Making your model Shiny!"), # title text colour
 # Sidebar controls
  sidebarLayout(                # indicates layout is going to be a sidebar-layout
   sidebarPanel(                              # open sidebar panel
      numericInput(inputId = "c_Trt_1",       # id of input, used in server
                   label = "SoC Treatment Cost", # label next to numeric input
                   value = 2278,              # default cost value for Treatment 2
                   min = 0),                  # minimum value allowed
      
      numericInput(inputId = "c_Trt_2",       # id of input, used in server
                   label = "Comparator Treatment Cost", # label next to numeric input
                   value = 2086.50,           # default cost value for Treatment 2
                   min = 0),                  # minimum value allowed
      
      sliderInput(inputId = "SI_n_age_min",   # id of input, used in server
                  label = "Baseline Age",     # label next to numeric input
                  value = 40,                 # initial value
                  min = 10,                   # minimum value allowed
                  max = 60),                  # maximum value allowed
      
      actionButton(inputId = "run_model",     # id of action button, used in server
                   label   = "Run model")     # action button label (on button)
    # close sidebarPanel
    ),
   # Create main panel
   mainPanel(
    # Create tabs for numeric and graphical summaries
      tabsetPanel(
       # Economic results tab
       tabPanel("Economic Results",  icon = icon("calculator"),
                # using titlePanel to add white space above plots
                titlePanel(
                 column(width = 12,
                        tableOutput(outputId = "SO_icer_table"))
                 )),
       # Graphical summaries tab
       tabPanel("Graphical Summaries", icon = icon("chart-line"),
                # using titlePanel to add white space above plots
                titlePanel(
                 # create fluidRow
                 fluidRow(
                  # set column width for each trace plot
                  column(width = 6, plotOutput(outputId = "VO_cohort_plot_SoC")), # SoC
                  column(width = 6, plotOutput(outputId = "VO_cohort_plot_NT")) # NT
                  )))
       ) # end of tabsetPanel
    ) # end of mainPanel
  ) # end of sidebarLayout
) # end of UI fluidPage

# Server function ---------------------------------------------------------
server <- function(input, output){   # server = function with two inputs
  
  observeEvent(input$run_model,       # when action button pressed
               ignoreNULL = F, {
                 
                 # Run model wrapper function with the Shiny inputs and store as data-frame 
                 df_model_res = f_wrapper(c_Trt_1 = input$c_Trt_1,
                                          c_Trt_2 = input$c_Trt_2,
                                          n_age_min = input$SI_n_age_min,
                                          n_sim = input$SI_n_sim)
                 
                 # Economic analysis --------------------------------------
                 # Creates table output of economic analysis results from wrapper function
                 output$SO_icer_table <- renderTable({ # this continuously updates table
                   # Create dataframe
                  df_res_table <- data.frame(
                   Treatment =  c("Comparator","Standard of Care"),
                   
                   LYs  =  c(df_model_res[[1]]["LYs_NT"], df_model_res[[1]]["LYs_SoC"]),
                   
                   Costs  =  c(df_model_res[[1]]["Cost_NT"], df_model_res[[1]]["Cost_SoC"]),
                   
                   Inc.LYs = c(df_model_res[[1]]["LYs_NT"] - df_model_res[[1]]["LYs_SoC"], NA),
                   
                   Inc.Costs = c(df_model_res[[1]]["Cost_NT"] - df_model_res[[1]]["Cost_SoC"], NA),
                   
                   ICER = c(df_model_res[[1]]["ICER"], NA)
                   )
                  
                  # print dataframe
                  df_res_table
                  
                 }, hover = TRUE, width = "100%", digits = 2, align = "c", 
                 striped = TRUE)
                 
                 # Visualisation outputs ---------------------------------------------------
                 # Plot of cohort trace for SoC
                 output$VO_cohort_plot_SoC <- renderPlot({ # render plot repeatedly updates
                  # Render cohort trace for SoC
                  df_model_res[[2]] + 
                   ggtitle("Comparator") + 
                   theme(plot.title = element_text(hjust = 0.5))
                  }) # Soc plot end
                  # Plot of cohort trace for NT
                 output$VO_cohort_plot_NT <- renderPlot({ # render plot repeatedly updates
                  # Render cohort trace for SoC
                  df_model_res[[3]] + 
                   ggtitle("Standard of Care") +
                   theme(plot.title = element_text(hjust = 0.5))
                  }) # Soc plot end
               }) # observe Event End
# Server end 
}

# Run app -----------------------------------------------------------------
shinyApp(ui, server)

# End file ----------------------------------------------------------------