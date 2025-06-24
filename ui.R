#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyWidgets)
library(bslib)
library(plotly)
library(shinyLP)
library(shinyfullscreen)
library(reshape2)
library(htmlwidgets)
library(tidyr)
library(dplyr)
library(formattable)

navbarPage(id = "tabs1",
           
           tags$head(
             tags$style(HTML("
                                    .shiny-output-error-validation {
                                                                    color: #ff0000;
                                                                    font-weight: bold;
                                                                   }
                                   ")
             )
           ),
           
           tags$head(tags$style(HTML("
    .number-box {
      display: inline-block;
      text-align: center;
      padding: 20px;
      font-size: 24px;
      font-weight: bold;
      color: black;
      background-color: lightblue;
      border-radius: 10px;
    }
  "))),
           
           
           title = "IRR Calculator",
           
           theme = bslib::bs_theme(bootswatch= "solar"),  
           
           tabPanel(
             "Home",
             fluidPage(
               div(
                 class = "text-center",
                 jumbotron(
                   header = "",
                   content = HTML('<br><br><br><br><div class="mt-8 mb-8"><p style="font-size:48px;">IRR Calculator App</p></div>
                                    <br><br><br><br> <img src="IAQG_Logo.png" height="200px" width="300px">
                                    <br><br><br><br> <p style="font-size:24px;">This App is Provided by IAQG To Facilitate the Use of AS9138 Copyright 2025 </p>'),
                   button = FALSE
                 )
               )
             )
           ),
           
           tabPanel("Tutorial",
                    sidebarLayout(
                      sidebarPanel(width = 2,
                                   
                                   radioButtons("rbs",
                                                label = "Tutorials",
                                                choices = c("Manual" = "manual"
                                                )
                                   )
                      ),
                      mainPanel(
                        fluidRow(
                          column(12, align = "center",
                                 conditionalPanel(
                                   condition = "input.rbs == 'manual'",
                                   titlePanel("IRR Calculator Manual"),
                                   tags$iframe(src = "book/index.html", width = "100%", height = "800px")  # Embed the book
                                   
                                 )
                          )
                        )
                      )
                    )
           ),
           
           # Panel for calculator ---
           tabPanel("Calculator",
                    sidebarLayout( 
                      sidebarPanel(width=3,
                                   
                                   br(),
                                   br(),
                                   hr(),
                                   tags$div("Managing IRR Inputs", style = "text-align: center; font-size: 28px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   hr(),
                                   actionButton("start_btn", "Start Analysis", 
                                                class = "btn-success btn-lg"),
                                   br(), 
                                   br(),
                                   
                                   tabsetPanel(
                                     tabPanel("PNC",
                                              
                                              br(),
                                              br(),
                                              hr(),
                                              tags$div("Probability of Failure Given Nonconformance Inputs", style = "text-align: center; font-size: 20px; font-weight: bold;"),
                                              hr(),
                                              br(),
                                              br(),
                                              
                                              radioButtons("eType_PNC",
                                                           label = "Uncertainty Type",
                                                           choices = c("Point Estimates - No Uncertainty" = "unknownUnc_PNC",
                                                                       "Epistemic Uncertainty" = "epistemicUnc_PNC",
                                                                       "Aleatoric Uncertainty" = "aleatoricUnc_PNC"
                                                           ),
                                                           selected = "unknownUnc_PNC"
                                              ),
                                              
                                              # Input: Numeric Value for Probability of Failure Given Nonconformance
                                              conditionalPanel(
                                                condition = "input.eType_PNC == 'unknownUnc_PNC'",
                                                numericInput(
                                                  "PF_NC",
                                                  "Probability of Failure given Nonconformance",
                                                  value = 0.5,
                                                  min = 0,
                                                  max = 1
                                                )
                                              ),
                                              conditionalPanel(
                                                condition = "input.eType_PNC == 'epistemicUnc_PNC'",
                                                numericInput(
                                                  "PF_NC_lower",
                                                  "Lower Boundary",
                                                  value = 0.5,
                                                  min = 0,
                                                  max = 1
                                                ),
                                                # Input: Numeric Value for Probability of Failure Given Nonconformance
                                                numericInput(
                                                  "PF_NC_upper",
                                                  "Upper Boundary",
                                                  value = 0.5,
                                                  min = 0,
                                                  max = 1
                                                )
                                              ),
                                              conditionalPanel(
                                                condition = "input.eType_PNC == 'aleatoricUnc_PNC'",
                                                radioButtons("PNC_in",
                                                             label = "Distribution Type",
                                                             choices = c("Nornal" = "PNC_norm",
                                                                         "Beta" = "PNC_beta",
                                                                         "Log-Normal" = "PNC_logn"
                                                             ),
                                                             selected = "PNC_norm"
                                                ),
                                                conditionalPanel(
                                                  condition = "input.PNC_in == 'PNC_norm'",
                                                  numericInput(
                                                    "PF_NC_mean",
                                                    "Mean for PNC",
                                                    value = 0.5,
                                                    min = 0,
                                                    max = 1
                                                  ),
                                                  # Input: Numeric Value for Probability of Failure Given Nonconformance
                                                  numericInput(
                                                    "PF_NC_sd",
                                                    "Standard Deviation for PNC",
                                                    value = 0.5,
                                                    min = 0,
                                                    max = 1
                                                  )
                                                ),
                                                conditionalPanel(
                                                  condition = "input.PNC_in == 'PNC_beta'",
                                                  numericInput(
                                                    "PF_NC_shape1",
                                                    "Shape 1 for PNC",
                                                    value = 0.5,
                                                    min = 0,
                                                    max = 1
                                                  ),
                                                  # Input: Numeric Value for Probability of Failure Given Nonconformance
                                                  numericInput(
                                                    "PF_NC_shape2",
                                                    "SHape 2 for PNC",
                                                    value = 0.5,
                                                    min = 0,
                                                    max = 1
                                                  )
                                                ),
                                                conditionalPanel(
                                                  condition = "input.PNC_in == 'PNC_logn'",
                                                  numericInput(
                                                    "PF_NC_lgMean",
                                                    "Mean - Log for PNC",
                                                    value = 0.5,
                                                    min = 0,
                                                    max = 1
                                                  ),
                                                  # Input: Numeric Value for Probability of Failure Given Nonconformance
                                                  numericInput(
                                                    "PF_NC_lgSD",
                                                    "Standard Deviation - Log for PNC",
                                                    value = 0.5,
                                                    min = 0,
                                                    max = 1
                                                  )
                                                )
                                              )
                                     ),
                                     
                                     tabPanel("PC",
                                              
                                              br(),
                                              br(),
                                              hr(),
                                              tags$div("Probability of Failure Given Conformance Inputs", style = "text-align: center; font-size: 20px; font-weight: bold;"),
                                              hr(),
                                              br(),
                                              br(),
                                              
                                              radioButtons("eType_PC",
                                                           label = "Uncertainty Type",
                                                           choices = c("Point Estimates - No Uncertainty" = "unknownUnc_PC",
                                                                       "Epistemic Uncertainty" = "epistemicUnc_PC",
                                                                       "Aleatoric Uncertainty" = "aleatoricUnc_PC"
                                                           ),
                                                           selected = "unknownUnc_PC"
                                              ),
                                              
                                              # Input: Numeric Value for Probability of Failure Given Conformance
                                              conditionalPanel(
                                                condition = "input.eType_PC == 'unknownUnc_PC'",
                                                numericInput(
                                                  "PF_C",
                                                  "Probability of Failure given Nonconformance",
                                                  value = 0.5,
                                                  min = 0,
                                                  max = 1
                                                )
                                              ),
                                              conditionalPanel(
                                                condition = "input.eType_PC == 'epistemicUnc_PC'",
                                                numericInput(
                                                  "PF_C_lower",
                                                  "Lower Boundary",
                                                  value = 0.5,
                                                  min = 0,
                                                  max = 1
                                                ),
                                                # Input: Numeric Value for Probability of Failure Given Nonconformance
                                                numericInput(
                                                  "PF_C_upper",
                                                  "Upper Boundary",
                                                  value = 0.5,
                                                  min = 0,
                                                  max = 1
                                                )
                                              ),
                                              conditionalPanel(
                                                condition = "input.eType_PC == 'aleatoricUnc_PC'",
                                                radioButtons("PC_in",
                                                             label = "Distribution Type",
                                                             choices = c("Nornal" = "PC_norm",
                                                                         "Beta" = "PC_beta",
                                                                         "Log-Normal" = "PC_logn"
                                                             ),
                                                             selected = "PC_norm"
                                                ),
                                                conditionalPanel(
                                                  condition = "input.PC_in == 'PC_norm'",
                                                  numericInput(
                                                    "PF_C_mean",
                                                    "Mean for PC",
                                                    value = 0.5,
                                                    min = 0,
                                                    max = 1
                                                  ),
                                                  # Input: Numeric Value for Probability of Failure Given Nonconformance
                                                  numericInput(
                                                    "PF_C_sd",
                                                    "Standard Deviation for PC",
                                                    value = 0.5,
                                                    min = 0,
                                                    max = 1
                                                  )
                                                ),
                                                conditionalPanel(
                                                  condition = "input.PC_in == 'PC_beta'",
                                                  numericInput(
                                                    "PF_C_shape1",
                                                    "Shape 1 for PC",
                                                    value = 0.5,
                                                    min = 0,
                                                    max = 1
                                                  ),
                                                  # Input: Numeric Value for Probability of Failure Given Nonconformance
                                                  numericInput(
                                                    "PF_C_shape2",
                                                    "Shape 2 for PC",
                                                    value = 0.5,
                                                    min = 0,
                                                    max = 1
                                                  )
                                                ),
                                                conditionalPanel(
                                                  condition = "input.PC_in == 'PC_logn'",
                                                  numericInput(
                                                    "PF_C_lgMean",
                                                    "Mean - Log for PC",
                                                    value = 0.5,
                                                    min = 0,
                                                    max = 1
                                                  ),
                                                  # Input: Numeric Value for Probability of Failure Given Nonconformance
                                                  numericInput(
                                                    "PF_C_lgSD",
                                                    "Standard Deviation - Log for PC",
                                                    value = 0.5,
                                                    min = 0,
                                                    max = 1
                                                  )
                                                )
                                              )
                                     ),
                                     tabPanel("PTARG",
                                              
                                              br(),
                                              br(),
                                              hr(),
                                              tags$div("Target Probability of Failure Inputs", style = "text-align: center; font-size: 20px; font-weight: bold;"),
                                              hr(),
                                              br(),
                                              br(),
                                              
                                              radioButtons("eType_PTARG",
                                                           label = "Uncertainty Type",
                                                           choices = c("Point Estimates - No Uncertainty" = "unknownUnc_PTARG",
                                                                       "Epistemic Uncertainty" = "epistemicUnc_PTARG",
                                                                       "Aleatoric Uncertainty" = "aleatoricUnc_PTARG"
                                                           ),
                                                           selected = "unknownUnc_PTARG"
                                              ),
                                              
                                              # Input: Numeric Value for Probability of Failure Given Nonconformance
                                              conditionalPanel(
                                                condition = "input.eType_PTARG == 'unknownUnc_PTARG'",
                                                numericInput(
                                                  "PF_TARG",
                                                  "Probability of Failure Target",
                                                  value = 0.1,
                                                  min = 0,
                                                  max = 0.1
                                                )
                                              ),
                                              conditionalPanel(
                                                condition = "input.eType_PTARG == 'epistemicUnc_PTARG'",
                                                numericInput(
                                                  "PF_TARG_lower",
                                                  "Lower Boundary",
                                                  value = 0.5,
                                                  min = 0,
                                                  max = 1
                                                ),
                                                # Input: Numeric Value for Probability of Failure Given Nonconformance
                                                numericInput(
                                                  "PF_TARG_upper",
                                                  "Upper Boundary",
                                                  value = 0.5,
                                                  min = 0,
                                                  max = 1
                                                )
                                              ),
                                              conditionalPanel(
                                                condition = "input.eType_PTARG == 'aleatoricUnc_PTARG'",
                                                radioButtons("TARG_in",
                                                             label = "Distribution Type",
                                                             choices = c("Nornal" = "TARG_norm",
                                                                         "Beta" = "TARG_beta",
                                                                         "Log-Normal" = "TARG_logn"
                                                             ),
                                                             selected = "TARG_norm"
                                                ),
                                                conditionalPanel(
                                                  condition = "input.TARG_in == 'TARG_norm'",
                                                  numericInput(
                                                    "PF_TARG_mean",
                                                    "Mean for Target Probability of Failure",
                                                    value = 0.1,
                                                    min = 0,
                                                    max = 1
                                                  ),
                                                  # Input: Numeric Value for Probability of Failure Given Nonconformance
                                                  numericInput(
                                                    "PF_TARG_sd",
                                                    "Standard Deviation for Taarget Probability of Failure",
                                                    value = 0.005,
                                                    min = 0,
                                                    max = 1
                                                  )
                                                ),
                                                conditionalPanel(
                                                  condition = "input.TARG_in == 'TARG_beta'",
                                                  numericInput(
                                                    "PF_TARG_shape1",
                                                    "Shape 1 for PNC",
                                                    value = 0.5,
                                                    min = 0,
                                                    max = 1
                                                  ),
                                                  # Input: Numeric Value for Probability of Failure Given Nonconformance
                                                  numericInput(
                                                    "PF_TARG_shape2",
                                                    "SHape 2 for PNC",
                                                    value = 0.5,
                                                    min = 0,
                                                    max = 1
                                                  )
                                                ),
                                                conditionalPanel(
                                                  condition = "input.TARG_in == 'TARG_logn'",
                                                  numericInput(
                                                    "PF_TARG_lgMean",
                                                    "Mean - Log for PNC",
                                                    value = 0.5,
                                                    min = 0,
                                                    max = 1
                                                  ),
                                                  # Input: Numeric Value for Probability of Failure Given Nonconformance
                                                  numericInput(
                                                    "PF_TARG_lgSD",
                                                    "Standard Deviation - Log for PNC",
                                                    value = 0.5,
                                                    min = 0,
                                                    max = 1
                                                  )
                                                )
                                              )
                                     )
                                   )
                      ),
                      mainPanel(
                        fluidRow(
                          
                          # System 1:  Each is a point estimate ##
                          conditionalPanel(
                            condition = "input.eType_PNC == 'unknownUnc_PNC' & input.eType_PC == 'unknownUnc_PC' & input.eType_PTARG == 'unknownUnc_PTARG' ",
                            column(8, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   hr(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   uiOutput("subtitle_s1"),
                                   hr(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Results", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("selectedVar"),
                                   br(),
                                   uiOutput("results_comment_s1"),
                                   hr(),
                                   
                                   # The Summary Section
                                   
                                   br(),
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("uncertainty_statements_s1"),
                                   br(),
                                   uiOutput("summary_comment_s1"),
                                   hr(),
                                   
                                   # The Recommendations
                                   
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("recommendation_statement_s1"),
                                   hr(),
                                   
                            )
                          ),
                          #System 2: PNC = point, PC = point, PTarg = epistemic  ##
                          conditionalPanel(
                            condition = "input.eType_PNC == 'unknownUnc_PNC' & input.eType_PC == 'unknownUnc_PC' & input.eType_PTARG == 'epistemicUnc_PTARG' ",
                            column(12, align="center", 
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   hr(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("subtitle_s2"),
                                   hr(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Results", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   
                                   tags$head(
                                     tags$style(
                                       "tr:nth-child(1) {font-weight: bold;}
                                              tr:nth-child(2) {font-weight: bold;}
                                               "
                                     )
                                   ),
                                   tableOutput("epi_PoF_table_s2"),
                                   br(),
                                   uiOutput("results_comment_s2"),
                                   br(),
                                   hr(),
                                   
                                   # The Summary
                                   
                                   br(),
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   tableOutput("epi_values_table_s2"),
                                   br(),
                                   uiOutput("uncertainty_statements_s2"),
                                   br(),
                                   uiOutput("summary_comment_s2"),
                                   br(),
                                   hr(),
                                   
                                   # The Recommendation
                                   
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("recommendation_statement_s2"),
                                   br(),
                                   hr(),
                                   
                            )
                          ),
                          #System 3:  PNC = point, PC = point, PTarg = aleatoric
                          conditionalPanel(
                            condition = "input.eType_PNC == 'unknownUnc_PNC' & input.eType_PC == 'unknownUnc_PC' & input.eType_PTARG == 'aleatoricUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("subtitle_s3"),
                                   hr(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Results", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   plotlyOutput(outputId = "aleatoric_hist_s3",
                                                width = "100%",
                                                height = 600),
                                   verbatimTextOutput("count_data_s3"),
                                   br(),
                                   uiOutput("results_comment_s3"),
                                   br(),
                                   hr(),
                                   
                                   # The Discussion Section
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   fluidRow(
                                     column(6, align="center",
                                            plotOutput(outputId = "ecdf_plot_s3",
                                                       width = "100%",
                                                       height = 600),
                                     ),
                                     column(6, align="center",
                                            tableOutput("quantile_data_s3"),
                                            br(),
                                            tableOutput("interval_data_s3")
                                     )
                                   ),
                                   br(),
                                   uiOutput("uncertainty_statements_s3"),
                                   br(),
                                   uiOutput("summary_comment_s3"),
                                   hr(),
                                   
                                   # The Recommendation Section
                                   
                                   br(),
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("recommendation_statement_s3"),
                                   hr(),
                                   
                            )
                          ),
                          
                          #System 4: PNC = point, PC = epistemic, PTarg = point
                          conditionalPanel(
                            condition = "input.eType_PNC == 'unknownUnc_PNC' & input.eType_PC == 'epistemicUnc_PC' & input.eType_PTARG == 'unknownUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   hr(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("subtitle_s4"),
                                   hr(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Results", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   tags$head(
                                     tags$style(
                                       "tr:nth-child(1) {font-weight: bold;}
                                              tr:nth-child(2) {font-weight: bold;}
                                               "
                                     )
                                   ),
                                   br(),
                                   tableOutput("epi_PoF_table_s4"),
                                   br(),
                                   uiOutput("results_comment_s4"),
                                   br(),
                                   hr(),
                                   
                                   # The Summary
                                   
                                   br(),
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   tableOutput("epi_values_table_s4"),
                                   br(),
                                   uiOutput("uncertainty_statements_s4"),
                                   br(),
                                   uiOutput("summary_comment_s4"),
                                   hr(),
                                   
                                   # The Recommendation
                                   
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("recommendation_statement_s4"),
                                   hr(),
                                   
                            )
                          ),
                          
                          #System 5: PNC = point, PC = epistemic, PTarg = epistemic
                          conditionalPanel(
                            condition = "input.eType_PNC == 'unknownUnc_PNC' & input.eType_PC == 'epistemicUnc_PC' & input.eType_PTARG == 'epistemicUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   hr(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("subtitle_s5"),
                                   hr(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Results", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   tags$head(
                                     tags$style(
                                       "tr:nth-child(1) {font-weight: bold;}
                                              tr:nth-child(2) {font-weight: bold;}
                                               "
                                     )
                                   ),
                                   br(),
                                   tableOutput("epi_PoF_table_s5"),
                                   br(),
                                   uiOutput("results_comment_s5"),
                                   br(),
                                   hr(),
                                   
                                   # The Summary
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   tableOutput("epi_values_table_s5"),
                                   br(),
                                   uiOutput("uncertainty_statements_s5"),
                                   br(),
                                   uiOutput("summary_comment_s5"),
                                   hr(),
                                   
                                   # The Recommendation
                                   
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("recommendation_statement_s5"),
                                   br(),
                                   hr(),
                                   
                            )
                          ),
                          #System 6: PNC = point, PC = epistemic, PTarg = aleatoric
                          conditionalPanel(
                            condition = "input.eType_PNC == 'unknownUnc_PNC' & input.eType_PC == 'epistemicUnc_PC' & input.eType_PTARG == 'aleatoricUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("subtitle_s6"),
                                   hr(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Results", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   plotlyOutput(outputId = "aleatoric_hist_s6",
                                                width = "100%",
                                                height = 600),
                                   verbatimTextOutput("count_data_s6"),
                                   br(),
                                   uiOutput("results_comment_s6"),
                                   br(),
                                   hr(),
                                   
                                   # The Summary Section
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   fluidRow(
                                     column(6, align="center",
                                            plotOutput(outputId = "ecdf_plot_s6",
                                                       width = "100%",
                                                       height = 600),
                                     ),
                                     column(6, align="center",
                                            tableOutput("quantile_data_s6"),
                                            br(),
                                            tableOutput("interval_data_s6")
                                     )
                                   ),
                                   br(),
                                   uiOutput("uncertainty_statements_s6"),
                                   br(),
                                   uiOutput("summary_comment_s6"),
                                   hr(),
                                   
                                   # The Recommendation Section
                                   
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("recommendation_statement_s6"),
                                   br(),
                                   hr(),
                                   
                            )
                          ),
                          
                          #System 7: PNC = point, PC = aleatoric, PTarg = point
                          conditionalPanel(
                            condition = "input.eType_PNC == 'unknownUnc_PNC' & input.eType_PC == 'aleatoricUnc_PC' & input.eType_PTARG == 'unknownUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("subtitle_s7"),
                                   hr(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Results", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   plotlyOutput(outputId = "aleatoric_hist_s7",
                                                width = "100%",
                                                height = 600),
                                   verbatimTextOutput("count_data_s7"),
                                   br(),
                                   uiOutput("results_comment_s7"),
                                   br(),
                                   hr(),
                                   
                                   # The Summary Section
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   fluidRow(
                                     column(6, align="center",
                                            plotOutput(outputId = "ecdf_plot_s7",
                                                       width = "100%",
                                                       height = 600),
                                     ),
                                     column(6, align="center",
                                            tableOutput("quantile_data_s7"),
                                            br(),
                                            tableOutput("interval_data_s7")
                                     )
                                   ),
                                   br(),
                                   uiOutput("uncertainty_statements_s7"),
                                   br(),
                                   uiOutput("summary_comment_s7"),
                                   hr(),
                                   
                                   # The Recommendation Section
                                   
                                   br(),
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("recommendation_statement_s7"),
                                   hr(),
                                   
                            )
                          ),
                          
                          #System 8: PNC = point, PC = aleatoric, PTarg = epistemic
                          conditionalPanel(
                            condition = "input.eType_PNC == 'unknownUnc_PNC' & input.eType_PC == 'aleatoricUnc_PC' & input.eType_PTARG == 'epistemicUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("subtitle_s8"),
                                   hr(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Results", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   plotlyOutput(outputId = "aleatoric_hist_s8",
                                                width = "100%",
                                                height = 600),
                                   verbatimTextOutput("count_data_s8"),
                                   br(),
                                   uiOutput("results_comment_s8"),
                                   br(),
                                   hr(),
                                   
                                   # The Summary Section
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   fluidRow(
                                     column(6, align="center",
                                            plotOutput(outputId = "ecdf_plot_s8",
                                                       width = "100%",
                                                       height = 600),
                                     ),
                                     column(6, align="center",
                                            tableOutput("quantile_data_s8"),
                                            br(),
                                            tableOutput("interval_data_s8")
                                     )
                                   ),
                                   br(),
                                   uiOutput("uncertainty_statements_s8"),
                                   br(),
                                   uiOutput("summary_comment_s8"),
                                   hr(),
                                   
                                   # The Recommendation Section
                                   
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("recommendation_statement_s8"),
                                   br(),
                                   hr(),
                                   
                            )
                          ),
                          
                          #System 9: PNC = point, PC = alleatoric, PTarg = aleatoric
                          conditionalPanel(
                            condition = "input.eType_PNC == 'unknownUnc_PNC' & input.eType_PC == 'aleatoricUnc_PC' & input.eType_PTARG == 'aleatoricUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("subtitle_s9"),
                                   hr(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Results", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   plotlyOutput(outputId = "aleatoric_hist_s9",
                                                width = "100%",
                                                height = 600),
                                   verbatimTextOutput("count_data_s9"),
                                   br(),
                                   uiOutput("results_comment_s9"),
                                   br(),
                                   hr(),
                                   
                                   # The Summary Section
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   fluidRow(
                                     column(6, align="center",
                                            plotOutput(outputId = "ecdf_plot_s9",
                                                       width = "100%",
                                                       height = 600),
                                     ),
                                     column(6, align="center",
                                            tableOutput("quantile_data_s9"),
                                            br(),
                                            tableOutput("interval_data_s9")
                                     )
                                   ),
                                   br(),
                                   uiOutput("uncertainty_statements_s9"),
                                   br(),
                                   uiOutput("summary_comment_s9"),
                                   hr(),
                                   
                                   # The Recommendation Section
                                   
                                   br(),
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("recommendation_statement_s9"),
                                   hr(),
                                   
                            )
                          ),
                          
                          
                          #System 10: PNC = epistemic, PC = point, PTarg = point
                          conditionalPanel(
                            condition = "input.eType_PNC == 'epistemicUnc_PNC' & input.eType_PC == 'unknownUnc_PC' & input.eType_PTARG == 'unknownUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   hr(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("subtitle_s10"),
                                   hr(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Results", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   
                                   tags$head(
                                     tags$style(
                                       "tr:nth-child(1) {font-weight: bold;}
                                              tr:nth-child(2) {font-weight: bold;}
                                               "
                                     )
                                   ),
                                   tableOutput("epi_PoF_table_s10"),
                                   br(),
                                   uiOutput("results_comment_s10"),
                                   br(),
                                   hr(),
                                   
                                   # The Summary
                                   
                                   br(),
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   tableOutput("epi_values_table_s10"),
                                   br(),
                                   uiOutput("uncertainty_statements_s10"),
                                   br(),
                                   uiOutput("summary_comment_s10"),
                                   br(),
                                   hr(),
                                   
                                   # The Recommendation
                                   
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("recommendation_statement_s10"),
                                   br(),
                                   hr(),
                                   
                            )
                          ),
                          
                          #System 11: PNC = epistemic, PC = point, PTarg = epistemic
                          conditionalPanel(
                            condition = "input.eType_PNC == 'epistemicUnc_PNC' & input.eType_PC == 'unknownUnc_PC' & input.eType_PTARG == 'epistemicUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   hr(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("subtitle_s11"),
                                   hr(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Results", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   
                                   tags$head(
                                     tags$style(
                                       "tr:nth-child(1) {font-weight: bold;}
                                              tr:nth-child(2) {font-weight: bold;}
                                               "
                                     )
                                   ),
                                   tableOutput("epi_PoF_table_s11"),
                                   br(),
                                   uiOutput("results_comment_s11"),
                                   br(),
                                   hr(),
                                   
                                   # The Summary
                                   
                                   br(),
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   tableOutput("epi_values_table_s11"),
                                   br(),
                                   uiOutput("uncertainty_statements_s11"),
                                   br(),
                                   uiOutput("summary_comment_s11"),
                                   br(),
                                   hr(),
                                   
                                   # The Recommendation
                                   
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("recommendation_statement_s11"),
                                   br(),
                                   hr(),
                                   
                            )
                          ),
                          
                          #System 12: PNC = epistemic, PC = point, PTarg = aleatoric
                          conditionalPanel(
                            condition = "input.eType_PNC == 'epistemicUnc_PNC' & input.eType_PC == 'unknownUnc_PC' & input.eType_PTARG == 'aleatoricUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("subtitle_s12"),
                                   hr(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Results", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   plotlyOutput(outputId = "aleatoric_hist_s12",
                                                width = "100%",
                                                height = 600),
                                   verbatimTextOutput("count_data_s12"),
                                   br(),
                                   uiOutput("results_comment_s12"),
                                   br(),
                                   hr(),
                                   
                                   # The Summary Section
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   fluidRow(
                                     column(6, align="center",
                                            plotOutput(outputId = "ecdf_plot_s12",
                                                       width = "100%",
                                                       height = 600),
                                     ),
                                     column(6, align="center",
                                            tableOutput("quantile_data_s12"),
                                            br(),
                                            tableOutput("interval_data_s12")
                                     )
                                   ),
                                   br(),
                                   uiOutput("uncertainty_statements_s12"),
                                   br(),
                                   uiOutput("summary_comment_s12"),
                                   hr(),
                                   
                                   # The Recommendation Section
                                   
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("recommendation_statement_s12"),
                                   br(),
                                   hr(),
                                   
                            )
                          ),
                          
                          #System 13: PNC = epistemic, PC = epistemic, PTarg = point
                          conditionalPanel(
                            condition = "input.eType_PNC == 'epistemicUnc_PNC' & input.eType_PC == 'epistemicUnc_PC' & input.eType_PTARG == 'unknownUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   hr(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("subtitle_s13"),
                                   hr(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Results", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   tags$head(
                                     tags$style(
                                       "tr:nth-child(1) {font-weight: bold;}
                                              tr:nth-child(2) {font-weight: bold;}
                                               "
                                     )
                                   ),
                                   br(),
                                   tableOutput("epi_PoF_table_s13"),
                                   br(),
                                   uiOutput("results_comment_s13"),
                                   br(),
                                   hr(),
                                   
                                   # The Summary
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   tableOutput("epi_values_table_s13"),
                                   br(),
                                   uiOutput("uncertainty_statements_s13"),
                                   br(),
                                   uiOutput("summary_comment_s13"),
                                   hr(),
                                   
                                   # The Recommendation
                                   
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("recommendation_statement_s13"),
                                   br(),
                                   hr(),
                                   
                            )
                          ),
                          
                          #System 14: PNC = epistemic, PC = epistemic, PTarg = epistemic
                          conditionalPanel(
                            condition = "input.eType_PNC == 'epistemicUnc_PNC' & input.eType_PC == 'epistemicUnc_PC' & input.eType_PTARG == 'epistemicUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   hr(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("subtitle_s14"),
                                   hr(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Results", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   tags$head(
                                     tags$style(
                                       "tr:nth-child(1) {font-weight: bold;}
                                              tr:nth-child(2) {font-weight: bold;}
                                               "
                                     )
                                   ),
                                   br(),
                                   tableOutput("epi_PoF_table_s14"),
                                   br(),
                                   uiOutput("results_comment_s14"),
                                   br(),
                                   hr(),
                                   
                                   # The Summary
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   tableOutput("epi_values_table_s14"),
                                   br(),
                                   uiOutput("uncertainty_statements_s14"),
                                   br(),
                                   uiOutput("summary_comment_s14"),
                                   hr(),
                                   
                                   # The Recommendation
                                   
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   uiOutput("recommendation_statement_s14"),
                                   br(),
                                   hr(),
                                   
                            )
                          ),
                          
                          #System 15: PNC = epistemic, PC = epistemic, PTarg = aleatoric
                          conditionalPanel(
                            condition = "input.eType_PNC == 'epistemicUnc_PNC' & input.eType_PC == 'epistemicUnc_PC' & input.eType_PTARG == 'aleatoricUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("PofF_NC is Epistemic, PofF_C is a point estimate, P Target is Aleatoric", style = "text-align: center; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Plot of Estimated IRR Values for Aleatoric Target and both PofF_NC & PofFF_C Epistemic", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   
                                   br(),
                                   br(),
                                   plotlyOutput(outputId = "aleatoric_hist_s15",
                                                width = "100%",
                                                height = 600),
                                   verbatimTextOutput("count_data_s15"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Summary Section
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   br(),
                                   tableOutput("quantile_data_s15"),
                                   br(),
                                   br(),
                                   
                                   tags$div("1.  The Probability of Failure given Nonconformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("2.  The Probability of Failure given Conformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("3.  This problem occurs where a target is defined based on current performance, and the distribution is known.  It helps to capture some of the uncertainty.  Remaining uncertainty will continue to exist for PNC and PC and will not be captured here. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   br(),
                                   tags$div("4.  We are able to establish a confidence level for the distribution as far as it is known. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("5.  But this estimate may still miss the mark in terms of over or underestimaing the needed IRR due to the point estimates used for PNC and PC.  We may still understimate the IRR and will not satisfy the actual need. The cost of inspection is reduced but at the risk that the product will not satisfy the actual need.  If the actual need falls below this range, then cost to inspect the product will increase. The product will more than satisfy the actual need, but at increased manufacturing costs.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("6.  As PofF_NC approachs 1, the IRR brecomes less sensitive to small changes, error will not have as significant effect in the IRR.  On the other had, as PofF_NC approaches P Target,the the IRR becomes much more sensitive to samll changes.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   
                                   # The Recommendation Section
                                   
                                   br(),
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("While this method begins to reveal more with regards to uncertainty in our estimated required IRR, it still is highly recommended that this value be applied only where there is a high tolerance for failure.  This should include target probabilities of failure that are > 1 x 10^(-3),  in the category of 'Likely Failure Condition'.  See Table 3.X in the Tutorial for ranges of Target Probability of Failure and appropriate labels.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                            )
                          ),
                          
                          #System 16: PNC = epistemic, PC = aleatoric, PTarg = point
                          conditionalPanel(
                            condition = "input.eType_PNC == 'epistemicUnc_PNC' & input.eType_PC == 'aleatoricUnc_PC' & input.eType_PTARG == 'unknownUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("PofF_NC is Epistemic, PofF_C is a point estimate, P Target is Aleatoric", style = "text-align: center; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Plot of Estimated IRR Values for PofF_NC as Epistemic and PofF_C Aleatoric", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   
                                   br(),
                                   br(),
                                   plotlyOutput(outputId = "aleatoric_hist_s16",
                                                width = "100%",
                                                height = 600),
                                   verbatimTextOutput("count_data_s16"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Summary Section
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   br(),
                                   tableOutput("quantile_data_s16"),
                                   br(),
                                   br(),
                                   
                                   tags$div("1.  The Probability of Failure given Nonconformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("2.  The Probability of Failure given Conformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("3.  This problem occurs where a target is defined based on current performance, and the distribution is known.  It helps to capture some of the uncertainty.  Remaining uncertainty will continue to exist for PNC and PC and will not be captured here. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   br(),
                                   tags$div("4.  We are able to establish a confidence level for the distribution as far as it is known. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("5.  But this estimate may still miss the mark in terms of over or underestimaing the needed IRR due to the point estimates used for PNC and PC.  We may still understimate the IRR and will not satisfy the actual need. The cost of inspection is reduced but at the risk that the product will not satisfy the actual need.  If the actual need falls below this range, then cost to inspect the product will increase. The product will more than satisfy the actual need, but at increased manufacturing costs.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("6.  As PofF_NC approachs 1, the IRR brecomes less sensitive to small changes, error will not have as significant effect in the IRR.  On the other had, as PofF_NC approaches P Target,the the IRR becomes much more sensitive to samll changes.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   
                                   # The Recommendation Section
                                   
                                   br(),
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("While this method begins to reveal more with regards to uncertainty in our estimated required IRR, it still is highly recommended that this value be applied only where there is a high tolerance for failure.  This should include target probabilities of failure that are > 1 x 10^(-3),  in the category of 'Likely Failure Condition'.  See Table 3.X in the Tutorial for ranges of Target Probability of Failure and appropriate labels.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                            )
                          ),
                          
                          #System 17: PNC = epistemic, PC = aleatoric, PTarg = epistemic
                          conditionalPanel(
                            condition = "input.eType_PNC == 'epistemicUnc_PNC' & input.eType_PC == 'aleatoricUnc_PC' & input.eType_PTARG == 'epistemicUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("PofF_NC is Epistemic, PofF_C is a Aleatoric, P Target is Epistemic", style = "text-align: center; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Plot of Estimated IRR Values for Aleatoric Target and both PofF_NC & PofFF_C Epistemic", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   
                                   br(),
                                   br(),
                                   plotlyOutput(outputId = "aleatoric_hist_s17",
                                                width = "100%",
                                                height = 600),
                                   verbatimTextOutput("count_data_s17"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Summary Section
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   br(),
                                   tableOutput("quantile_data_s17"),
                                   br(),
                                   br(),
                                   
                                   tags$div("1.  The Probability of Failure given Nonconformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("2.  The Probability of Failure given Conformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("3.  This problem occurs where a target is defined based on current performance, and the distribution is known.  It helps to capture some of the uncertainty.  Remaining uncertainty will continue to exist for PNC and PC and will not be captured here. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   br(),
                                   tags$div("4.  We are able to establish a confidence level for the distribution as far as it is known. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("5.  But this estimate may still miss the mark in terms of over or underestimaing the needed IRR due to the point estimates used for PNC and PC.  We may still understimate the IRR and will not satisfy the actual need. The cost of inspection is reduced but at the risk that the product will not satisfy the actual need.  If the actual need falls below this range, then cost to inspect the product will increase. The product will more than satisfy the actual need, but at increased manufacturing costs.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("6.  As PofF_NC approachs 1, the IRR brecomes less sensitive to small changes, error will not have as significant effect in the IRR.  On the other had, as PofF_NC approaches P Target,the the IRR becomes much more sensitive to samll changes.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   
                                   # The Recommendation Section
                                   
                                   br(),
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("While this method begins to reveal more with regards to uncertainty in our estimated required IRR, it still is highly recommended that this value be applied only where there is a high tolerance for failure.  This should include target probabilities of failure that are > 1 x 10^(-3),  in the category of 'Likely Failure Condition'.  See Table 3.X in the Tutorial for ranges of Target Probability of Failure and appropriate labels.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                            )
                          ),
                          
                          #System 18: PNC = epistemic, PC = aleatoric, PTarg = aleatoric
                          conditionalPanel(
                            condition = "input.eType_PNC == 'epistemicUnc_PNC' & input.eType_PC == 'aleatoricUnc_PC' & input.eType_PTARG == 'aleatoricUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("PNC is Epistemic, PC and P Target are Aleatoric", style = "text-align: center; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Plot of Estimated IRR Values for Aleatoric Target", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   
                                   br(),
                                   br(),
                                   plotlyOutput(outputId = "aleatoric_hist_s18",
                                                width = "100%",
                                                height = 600),
                                   verbatimTextOutput("count_data_s18"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Summary Section
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   br(),
                                   tableOutput("quantile_data_s18"),
                                   br(),
                                   br(),
                                   
                                   tags$div("1.  The Probability of Failure given Nonconformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("2.  The Probability of Failure given Conformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("3.  This problem occurs where a target is defined based on current performance, and the distribution is known.  It helps to capture some of the uncertainty.  Remaining uncertainty will continue to exist for PNC and PC and will not be captured here. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   br(),
                                   tags$div("4.  We are able to establish a confidence level for the distribution as far as it is known. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("5.  But this estimate may still miss the mark in terms of over or underestimaing the needed IRR due to the point estimates used for PNC and PC.  We may still understimate the IRR and will not satisfy the actual need. The cost of inspection is reduced but at the risk that the product will not satisfy the actual need.  If the actual need falls below this range, then cost to inspect the product will increase. The product will more than satisfy the actual need, but at increased manufacturing costs.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   
                                   # The Recommendation Section
                                   
                                   br(),
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("While this method begins to reveal more with regards to uncertainty in our estimated required IRR, it still is highly recommended that this value be applied only where there is a high tolerance for failure.  This should include target probabilities of failure that are > 1 x 10^(-3),  in the category of 'Likely Failure Condition'.  See Table 3.X in the Tutorial for ranges of Target Probability of Failure and appropriate labels.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                            )
                          ),
                          
                          #System 19: PNC = aleatoric, PC = point, PTarg = point
                          conditionalPanel(
                            condition = "input.eType_PNC == 'aleatoricUnc_PNC' & input.eType_PC == 'unknownUnc_PC' & input.eType_PTARG == 'unknownUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("PC and PoF are point estimates, PNC is Aleatoric", style = "text-align: center; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Plot of Estimated IRR Values for Aleatoric PNC", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   
                                   br(),
                                   br(),
                                   plotlyOutput(outputId = "aleatoric_hist_s19",
                                                width = "100%",
                                                height = 600),
                                   verbatimTextOutput("count_data_s19"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Summary Section
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   br(),
                                   tableOutput("quantile_data_s19"),
                                   br(),
                                   br(),
                                   
                                   tags$div("1.  The Probability of Failure given Nonconformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("2.  The Probability of Failure given Conformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("3.  This problem occurs where a target is defined based on current performance, and the distribution is known.  It helps to capture some of the uncertainty.  Remaining uncertainty will continue to exist for PNC and PC and will not be captured here. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   br(),
                                   tags$div("4.  We are able to establish a confidence level for the distribution as far as it is known. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("5.  But this estimate may still miss the mark in terms of over or underestimaing the needed IRR due to the point estimates used for PNC and PC.  We may still understimate the IRR and will not satisfy the actual need. The cost of inspection is reduced but at the risk that the product will not satisfy the actual need.  If the actual need falls below this range, then cost to inspect the product will increase. The product will more than satisfy the actual need, but at increased manufacturing costs.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   
                                   # The Recommendation Section
                                   
                                   br(),
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("While this method begins to reveal more with regards to uncertainty in our estimated required IRR, it still is highly recommended that this value be applied only where there is a high tolerance for failure.  This should include target probabilities of failure that are > 1 x 10^(-3),  in the category of 'Likely Failure Condition'.  See Table 3.X in the Tutorial for ranges of Target Probability of Failure and appropriate labels.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                            )
                          ),
                          
                          #System 20: PNC = aleatoric, PC = point, PTarg = epistemic
                          conditionalPanel(
                            condition = "input.eType_PNC == 'aleatoricUnc_PNC' & input.eType_PC == 'unknownUnc_PC' & input.eType_PTARG == 'epistemicUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("PofF_NC is a point estimate, PofF_C is Epistemic, P Target is Aleatoric", style = "text-align: center; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Plot of Estimated IRR Values for Aleatoric PNC", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   
                                   br(),
                                   br(),
                                   plotlyOutput(outputId = "aleatoric_hist_s20",
                                                width = "100%",
                                                height = 600),
                                   verbatimTextOutput("count_data_s20"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Summary Section
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   br(),
                                   tableOutput("quantile_data_s20"),
                                   br(),
                                   br(),
                                   
                                   tags$div("1.  The Probability of Failure given Nonconformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("2.  The Probability of Failure given Conformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("3.  This problem occurs where a target is defined based on current performance, and the distribution is known.  It helps to capture some of the uncertainty.  Remaining uncertainty will continue to exist for PNC and PC and will not be captured here. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   br(),
                                   tags$div("4.  We are able to establish a confidence level for the distribution as far as it is known. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("5.  But this estimate may still miss the mark in terms of over or underestimaing the needed IRR due to the point estimates used for PNC and PC.  We may still understimate the IRR and will not satisfy the actual need. The cost of inspection is reduced but at the risk that the product will not satisfy the actual need.  If the actual need falls below this range, then cost to inspect the product will increase. The product will more than satisfy the actual need, but at increased manufacturing costs.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("6.  As PofF_NC approachs 1, the IRR brecomes less sensitive to small changes, error will not have as significant effect in the IRR.  On the other had, as PofF_NC approaches P Target,the the IRR becomes much more sensitive to samll changes.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   
                                   # The Recommendation Section
                                   
                                   br(),
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("While this method begins to reveal more with regards to uncertainty in our estimated required IRR, it still is highly recommended that this value be applied only where there is a high tolerance for failure.  This should include target probabilities of failure that are > 1 x 10^(-3),  in the category of 'Likely Failure Condition'.  See Table 3.X in the Tutorial for ranges of Target Probability of Failure and appropriate labels.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                            )
                          ),
                          
                          #System 21: PNC = aleatoric, PC = point, PTarg = aleatoric
                          conditionalPanel(
                            condition = "input.eType_PNC == 'aleatoricUnc_PNC' & input.eType_PC == 'unknownUnc_PC' & input.eType_PTARG == 'aleatoricUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("PNC and PoF areAleatoric, PC is a point Estimate", style = "text-align: center; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Plot of Estimated IRR Values for Aleatoric Target", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   
                                   br(),
                                   br(),
                                   plotlyOutput(outputId = "aleatoric_hist_s21",
                                                width = "100%",
                                                height = 600),
                                   verbatimTextOutput("count_data_s21"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Summary Section
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   br(),
                                   tableOutput("quantile_data_s21"),
                                   br(),
                                   br(),
                                   
                                   tags$div("1.  The Probability of Failure given Nonconformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("2.  The Probability of Failure given Conformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("3.  This problem occurs where a target is defined based on current performance, and the distribution is known.  It helps to capture some of the uncertainty.  Remaining uncertainty will continue to exist for PNC and PC and will not be captured here. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   br(),
                                   tags$div("4.  We are able to establish a confidence level for the distribution as far as it is known. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("5.  But this estimate may still miss the mark in terms of over or underestimaing the needed IRR due to the point estimates used for PNC and PC.  We may still understimate the IRR and will not satisfy the actual need. The cost of inspection is reduced but at the risk that the product will not satisfy the actual need.  If the actual need falls below this range, then cost to inspect the product will increase. The product will more than satisfy the actual need, but at increased manufacturing costs.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   
                                   # The Recommendation Section
                                   
                                   br(),
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("While this method begins to reveal more with regards to uncertainty in our estimated required IRR, it still is highly recommended that this value be applied only where there is a high tolerance for failure.  This should include target probabilities of failure that are > 1 x 10^(-3),  in the category of 'Likely Failure Condition'.  See Table 3.X in the Tutorial for ranges of Target Probability of Failure and appropriate labels.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                            )
                          ),
                          
                          #System 22: PNC = aleatoric, PC = epistemic, PTarg = point
                          conditionalPanel(
                            condition = "input.eType_PNC == 'aleatoricUnc_PNC' & input.eType_PC == 'epistemicUnc_PC' & input.eType_PTARG == 'unknownUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("PNC is Aleatoric, PC is Epistemic, P Target is a point value", style = "text-align: center; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Plot of Estimated IRR Values for Aleatoric PNC", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   
                                   br(),
                                   br(),
                                   plotlyOutput(outputId = "aleatoric_hist_s22",
                                                width = "100%",
                                                height = 600),
                                   verbatimTextOutput("count_data_s22"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Summary Section
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   br(),
                                   tableOutput("quantile_data_s22"),
                                   br(),
                                   br(),
                                   
                                   tags$div("1.  The Probability of Failure given Nonconformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("2.  The Probability of Failure given Conformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("3.  This problem occurs where a target is defined based on current performance, and the distribution is known.  It helps to capture some of the uncertainty.  Remaining uncertainty will continue to exist for PNC and PC and will not be captured here. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   br(),
                                   tags$div("4.  We are able to establish a confidence level for the distribution as far as it is known. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("5.  But this estimate may still miss the mark in terms of over or underestimaing the needed IRR due to the point estimates used for PNC and PC.  We may still understimate the IRR and will not satisfy the actual need. The cost of inspection is reduced but at the risk that the product will not satisfy the actual need.  If the actual need falls below this range, then cost to inspect the product will increase. The product will more than satisfy the actual need, but at increased manufacturing costs.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("6.  As PofF_NC approachs 1, the IRR brecomes less sensitive to small changes, error will not have as significant effect in the IRR.  On the other had, as PofF_NC approaches P Target,the the IRR becomes much more sensitive to samll changes.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   
                                   # The Recommendation Section
                                   
                                   br(),
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("While this method begins to reveal more with regards to uncertainty in our estimated required IRR, it still is highly recommended that this value be applied only where there is a high tolerance for failure.  This should include target probabilities of failure that are > 1 x 10^(-3),  in the category of 'Likely Failure Condition'.  See Table 3.X in the Tutorial for ranges of Target Probability of Failure and appropriate labels.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                            )
                          ),
                          
                          #System 23: PNC = aleatoric, PC = epistemic, PTarg = epistemic
                          conditionalPanel(
                            condition = "input.eType_PNC == 'aleatoricUnc_PNC' & input.eType_PC == 'epistemicUnc_PC' & input.eType_PTARG == 'epistemicUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("PNC is Aleatoric, PC is a point estimate, PoF is Epistemic", style = "text-align: center; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Plot of Estimated IRR Values for Aleatoric Target and both PofF_NC & PofFF_C Epistemic", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   
                                   br(),
                                   br(),
                                   plotlyOutput(outputId = "aleatoric_hist_s23",
                                                width = "100%",
                                                height = 600),
                                   verbatimTextOutput("count_data_s23"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Summary Section
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   br(),
                                   tableOutput("quantile_data_s23"),
                                   br(),
                                   br(),
                                   
                                   tags$div("1.  The Probability of Failure given Nonconformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("2.  The Probability of Failure given Conformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("3.  This problem occurs where a target is defined based on current performance, and the distribution is known.  It helps to capture some of the uncertainty.  Remaining uncertainty will continue to exist for PNC and PC and will not be captured here. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   br(),
                                   tags$div("4.  We are able to establish a confidence level for the distribution as far as it is known. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("5.  But this estimate may still miss the mark in terms of over or underestimaing the needed IRR due to the point estimates used for PNC and PC.  We may still understimate the IRR and will not satisfy the actual need. The cost of inspection is reduced but at the risk that the product will not satisfy the actual need.  If the actual need falls below this range, then cost to inspect the product will increase. The product will more than satisfy the actual need, but at increased manufacturing costs.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("6.  As PofF_NC approachs 1, the IRR brecomes less sensitive to small changes, error will not have as significant effect in the IRR.  On the other had, as PofF_NC approaches P Target,the the IRR becomes much more sensitive to samll changes.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   
                                   # The Recommendation Section
                                   
                                   br(),
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("While this method begins to reveal more with regards to uncertainty in our estimated required IRR, it still is highly recommended that this value be applied only where there is a high tolerance for failure.  This should include target probabilities of failure that are > 1 x 10^(-3),  in the category of 'Likely Failure Condition'.  See Table 3.X in the Tutorial for ranges of Target Probability of Failure and appropriate labels.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                            )
                          ),
                          
                          #System 24: PNC = aleatoric, PC = epistemic, PTarg = aleatoric
                          conditionalPanel(
                            condition = "input.eType_PNC == 'aleatoricUnc_PNC' & input.eType_PC == 'epistemicUnc_PC' & input.eType_PTARG == 'aleatoricUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("PNC is Aleatoric, PC is Epistemic, and PoF is Aleatoric", style = "text-align: center; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Plot of Estimated IRR Values for Aleatoric PNC and PoF", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   
                                   br(),
                                   br(),
                                   plotlyOutput(outputId = "aleatoric_hist_s24",
                                                width = "100%",
                                                height = 600),
                                   verbatimTextOutput("count_data_s24"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Summary Section
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   br(),
                                   tableOutput("quantile_data_s24"),
                                   br(),
                                   br(),
                                   
                                   tags$div("1.  The Probability of Failure given Nonconformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("2.  The Probability of Failure given Conformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("3.  This problem occurs where a target is defined based on current performance, and the distribution is known.  It helps to capture some of the uncertainty.  Remaining uncertainty will continue to exist for PNC and PC and will not be captured here. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   br(),
                                   tags$div("4.  We are able to establish a confidence level for the distribution as far as it is known. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("5.  But this estimate may still miss the mark in terms of over or underestimaing the needed IRR due to the point estimates used for PNC and PC.  We may still understimate the IRR and will not satisfy the actual need. The cost of inspection is reduced but at the risk that the product will not satisfy the actual need.  If the actual need falls below this range, then cost to inspect the product will increase. The product will more than satisfy the actual need, but at increased manufacturing costs.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   
                                   # The Recommendation Section
                                   
                                   br(),
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("While this method begins to reveal more with regards to uncertainty in our estimated required IRR, it still is highly recommended that this value be applied only where there is a high tolerance for failure.  This should include target probabilities of failure that are > 1 x 10^(-3),  in the category of 'Likely Failure Condition'.  See Table 3.X in the Tutorial for ranges of Target Probability of Failure and appropriate labels.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                            )
                          ),
                          
                          #System 25: PNC = aleatoric, PC = aleatoric, PTarg = point
                          conditionalPanel(
                            condition = "input.eType_PNC == 'aleatoricUnc_PNC' & input.eType_PC == 'aleatoricUnc_PC' & input.eType_PTARG == 'unknownUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("PNC and PC are Aleatoric, PoF is a point value", style = "text-align: center; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Plot of Estimated IRR Values for Aleatoric PNC and PC", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   
                                   br(),
                                   br(),
                                   plotlyOutput(outputId = "aleatoric_hist_s25",
                                                width = "100%",
                                                height = 600),
                                   verbatimTextOutput("count_data_s25"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Summary Section
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   br(),
                                   tableOutput("quantile_data_s25"),
                                   br(),
                                   br(),
                                   
                                   tags$div("1.  The Probability of Failure given Nonconformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("2.  The Probability of Failure given Conformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("3.  This problem occurs where a target is defined based on current performance, and the distribution is known.  It helps to capture some of the uncertainty.  Remaining uncertainty will continue to exist for PNC and PC and will not be captured here. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   br(),
                                   tags$div("4.  We are able to establish a confidence level for the distribution as far as it is known. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("5.  But this estimate may still miss the mark in terms of over or underestimaing the needed IRR due to the point estimates used for PNC and PC.  We may still understimate the IRR and will not satisfy the actual need. The cost of inspection is reduced but at the risk that the product will not satisfy the actual need.  If the actual need falls below this range, then cost to inspect the product will increase. The product will more than satisfy the actual need, but at increased manufacturing costs.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   
                                   # The Recommendation Section
                                   
                                   br(),
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("While this method begins to reveal more with regards to uncertainty in our estimated required IRR, it still is highly recommended that this value be applied only where there is a high tolerance for failure.  This should include target probabilities of failure that are > 1 x 10^(-3),  in the category of 'Likely Failure Condition'.  See Table 3.X in the Tutorial for ranges of Target Probability of Failure and appropriate labels.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                            )
                          ),
                          
                          #System 26: PNC = aleatoric, PC = aleatoric, PTarg = epistemic
                          conditionalPanel(
                            condition = "input.eType_PNC == 'aleatoricUnc_PNC' & input.eType_PC == 'aleatoricUnc_PC' & input.eType_PTARG == 'epistemicUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("PNC is Aleatoric, PC is Aleatoric, and PoF is Epistemic", style = "text-align: center; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Plot of Estimated IRR Values for Aleatoric PNC and PoF", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   
                                   br(),
                                   br(),
                                   plotlyOutput(outputId = "aleatoric_hist_s26",
                                                width = "100%",
                                                height = 600),
                                   verbatimTextOutput("count_data_s26"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Summary Section
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   br(),
                                   tableOutput("quantile_data_s26"),
                                   br(),
                                   br(),
                                   
                                   tags$div("1.  The Probability of Failure given Nonconformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("2.  The Probability of Failure given Conformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("3.  This problem occurs where a target is defined based on current performance, and the distribution is known.  It helps to capture some of the uncertainty.  Remaining uncertainty will continue to exist for PNC and PC and will not be captured here. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   br(),
                                   tags$div("4.  We are able to establish a confidence level for the distribution as far as it is known. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("5.  But this estimate may still miss the mark in terms of over or underestimaing the needed IRR due to the point estimates used for PNC and PC.  We may still understimate the IRR and will not satisfy the actual need. The cost of inspection is reduced but at the risk that the product will not satisfy the actual need.  If the actual need falls below this range, then cost to inspect the product will increase. The product will more than satisfy the actual need, but at increased manufacturing costs.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   
                                   # The Recommendation Section
                                   
                                   br(),
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("While this method begins to reveal more with regards to uncertainty in our estimated required IRR, it still is highly recommended that this value be applied only where there is a high tolerance for failure.  This should include target probabilities of failure that are > 1 x 10^(-3),  in the category of 'Likely Failure Condition'.  See Table 3.X in the Tutorial for ranges of Target Probability of Failure and appropriate labels.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                            )
                          ),
                          
                          #System 27: PNC = aleatoric, PC = aleatoric, PTarg = aleatoric
                          conditionalPanel(
                            condition = "input.eType_PNC == 'aleatoricUnc_PNC' & input.eType_PC == 'aleatoricUnc_PC' & input.eType_PTARG == 'aleatoricUnc_PTARG' ",
                            column(12, align="center",
                                   
                                   # The Title Section
                                   
                                   br(),
                                   br(),
                                   tags$div("IRR Estimates", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("PNC is Aleatoric, PC is Aleatoric, and PoF is Aleatoric", style = "text-align: center; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Results Section
                                   
                                   br(),
                                   tags$div("Plot of Estimated IRR Values for Aleatoric PNC, PC and PoF", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   
                                   br(),
                                   br(),
                                   plotlyOutput(outputId = "aleatoric_hist_s27",
                                                width = "100%",
                                                height = 600),
                                   verbatimTextOutput("count_data_s27"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                                   # The Summary Section
                                   
                                   tags$div("Discussion", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   br(),
                                   br(),
                                   tableOutput("quantile_data_s27"),
                                   br(),
                                   br(),
                                   
                                   tags$div("1.  The Probability of Failure given Nonconformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("2.  The Probability of Failure given Conformance is based on judgment without any knowledge of the associated uncertainty. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("3.  This problem occurs where a target is defined based on current performance, and the distribution is known.  It helps to capture some of the uncertainty.  Remaining uncertainty will continue to exist for PNC and PC and will not be captured here. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   br(),
                                   tags$div("4.  We are able to establish a confidence level for the distribution as far as it is known. ", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   tags$div("5.  But this estimate may still miss the mark in terms of over or underestimaing the needed IRR due to the point estimates used for PNC and PC.  We may still understimate the IRR and will not satisfy the actual need. The cost of inspection is reduced but at the risk that the product will not satisfy the actual need.  If the actual need falls below this range, then cost to inspect the product will increase. The product will more than satisfy the actual need, but at increased manufacturing costs.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   
                                   # The Recommendation Section
                                   
                                   br(),
                                   br(),
                                   tags$div("Recommendation", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                   tags$div("While this method begins to reveal more with regards to uncertainty in our estimated required IRR, it still is highly recommended that this value be applied only where there is a high tolerance for failure.  This should include target probabilities of failure that are > 1 x 10^(-3),  in the category of 'Likely Failure Condition'.  See Table 3.X in the Tutorial for ranges of Target Probability of Failure and appropriate labels.", style = "text-align: left; font-size: 20px; font-weight: bold;"),
                                   hr(),
                                   br(),
                                   br(),
                                   
                            )
                          )
                        )
                      )
                    )           
           ),
           
           # Panel with contour plot ----
           tabPanel("Contour Plot", 
                    
                    sidebarLayout( 
                      sidebarPanel(width=3,
                                   
                                   # Input: Numeric Value for Minimum IRR Plot Display
                                   numericInput( 
                                     "minScale", 
                                     "Mininum IRR", 
                                     value = 0.5, 
                                     min = 0, 
                                     max = 1 
                                   ),   
                                   
                                   radioButtons("rbsTarget",
                                                label = "Target Entry",
                                                choices = c(
                                                  "Select List" = "select",
                                                  "Manual Input" = "manual"
                                                )
                                   ),
                                   
                                   conditionalPanel(
                                     condition = "input.rbsTarget == 'select'",
                                     radioButtons("rbsSelect",
                                                  label = "Target Selection",
                                                  choices = c("1 X 10-1" = 0.1,
                                                              "1 X 10-2" = 0.01,
                                                              "1 X 10-3" = 0.001,
                                                              "1 X 10-4" = 0.0001,
                                                              "1 X 10-5" = 0.00001,
                                                              "1 X 10-6" = 0.000001,
                                                              "1 X 10-7" = 0.0000001,
                                                              "1 X 10-8" = 0.00000001,
                                                              "1 X 10-9" = 0.000000001)
                                     ),
                                     #Ptarget = input$rbsSelect
                                   ),
                                   
                                   conditionalPanel(
                                     condition = "input.rbsTarget == 'manual'",
                                     numericInput(
                                       "Ptarget",
                                       "Target Maximum Probability of Failure Manual Input",
                                       value = 0.1,
                                       min = 0,
                                       max = 1
                                     ),
                                   ),
                                   
                                   # Evaluate a selected IRR
                                   
                                   checkboxInput(inputId = "cbEval",
                                                 label = "Evaluate a specified IRR"#,
                                                 #value = FALSE
                                   ),
                                   
                                   conditionalPanel(
                                     condition = "input.cbEval",
                                     numericInput(
                                       "selIRR",
                                       "IRR To Be Evaluated",
                                       value = 0.9,
                                       min = 0,
                                       max = 1
                                     ),
                                     
                                   )
                      ),
                      mainPanel(
                        fluidRow(
                          conditionalPanel(
                            condition = "input$tabs1 == 'Contour Plot'",
                            fluidRow(
                              plotlyOutput(outputId = "contPlot",
                                           width = "100%",
                                           height = 600)
                            ),
                            fluidRow(
                              column(12, align="center",
                                     wellPanel(
                                       h4("Current Values"),
                                       tableOutput("current_values")
                                     )
                              )
                            )
                          )
                        ),
                        br(),
                        br(),
                        fluidRow(
                          conditionalPanel(
                            condition = "input$cbEval",
                            plotlyOutput(outputId = "contPlot2",
                                         width = "100%",
                                         height = 600)
                          )
                        ),
                        br(),
                        br(),
                        br()
                      )
                    )
                    
           ) 
           
           # Contour Plot of IRR Region
)
