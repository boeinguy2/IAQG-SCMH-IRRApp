#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
# Define server logic required create output ----

function(input, output, session) {
  
  #############################################################################################
  
  ## For tutorial Panel - Main
  
  shiny::addResourcePath("book", "bookdown/_book")
  
  ## Setup Start Button
  
  # Handle start button click
  started <- reactiveVal(FALSE)
  
  # list for making subtitles
  
  type_list <- c("point estimate", "epistemic estimate", "aleatoric estimate")
  
  # List for making results comments
  
  results_comment_list <- c(
    "This result combines the three point estimates, but cannot assess any uncertainty associated with the IRR estimate.",
    "This table includes the minimum and maximum IRR produced in the bounded range for the Target Probability of Failure.  To the degree that the region contains the actual IRR needed, the least risk approach would be to select the maximum for this application.",
    " ",
    "This table includes the minimum and maximum IRR produced in the bounded range for the Probability of Failure given Conformance.  To the degree that the region contains the actual IRR needed, the least risk approach would be to select the maximum for this application.",
    "This table includes the minimum and maximum IRR produced in the bounded range for the Probability of Failure given Conformance.  To the degree that the region contains the actual IRR needed, the least risk approach would be to select the maximum for this application.",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    "This system has two epistemic inputs, each at two levels.  The result combines the two, creating 4 combinations.  The result then contains 4 histograms, one for each combination.  ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " "
  )
  
  # List for making Uncertainty comments
  
  uncertainty_list <- c(
    "PNC is a point estimate.  It is a subjective probability based on judgement.  At best this is is derived using a strucutred process, at worst it is a shot in the dark.",
    "PC is a point estimate.  It is a subjective probability based on judgement.  At best this is is derived using a strucutred process, at worst it is a shot in the dark.",
    "Target is a point estimate.  It may be selectd based on a regulatory or business requirement.  It may also be supplied based on subjective judgment.",
    "PNC is a bounded condition.  It is a subjective probability based on judgement.  The boundaries are set to capture the uncertainty that the Engineer believes represents contains the true value.  Uncertainty still exists if the boundaries are mispecified.",
    "PC is a bounded condition.  It is a subjective probability based on judgement.  The boundaries are set to capture the uncertainty that the Engineer believes represents contains the true value.  Uncertainty still exists if the boundaries are mispecified.",
    "Target is a bounded set of conditions.  It applies when the Enginer is not certain where the target may lie.",
    "PNC is represented by a probabiloity distribution that is modeled to simulate the distribution of possible outcomes.  This distribution is used to propagate the uncertianty into the IRR estimate.",
    "PC is represented by a probabiloity distribution that is modeled to simulate the distribution of possible outcomes.  This distribution is used to propagate the uncertianty into the IRR estimate.",
    "Target is represented by a probability distribution.  This is most useful when product data is used to set the Target and not a specified value supplied by regulators."
  )
  
  # List for making summary comments
  
  summary_comment_list <- c(
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " "
  )
  
  # List for making recommendations
  
  recommendation_list <- c(
    "Based on the existence of three subjective values, this results should be applied with caution.  Validation of the results should be required to capture any risk that the product may have a higher probability of failure than intended.  It is highly recommended that this value be applied only where there is a high tolerance for failure.  This should include target probabilities of failure that are > 1 x 10^(-3),  in the category of 'Likely Failure Condition'.  See Table 3.X in the Tutorial for ranges of Target Probability of Failure and appropriate labels.",
    "Based on the existence of three epistemic values, this results should be applied with caution.  Validation of the results should be required to capture any risk that the product may have a higher probability of failure than intended.  It is highly recommended that this value be applied only where there is a moderate tolerance for failure.  This should include target probabilities of failure that are > 1 x 10^(-5),  in the category of 'Likely Failure Condition'.  See Table 3.X in the Tutorial for ranges of Target Probability of Failure and appropriate labels."
  )
  
  #############################################################################################
  
  ## For Calculator Panel - Main
  
  observeEvent(input$start_btn, {
    
    started(TRUE)
    
    ## System 1                    ##
    ## PNC = Point                 ##
    ## PC = Point                  ##
    ## PTARG = Point               ##
    
    ## Create Subtitle
    
    output$subtitle_s1 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is a",type_list[1], ", PC is a", type_list[1], ", Target is a",type_list[1]))
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })  
    
    ## Create data
    
    analysis_results_1 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        PNC <- isolate(input$PF_NC)
        PC <- isolate(input$PF_C)
        PTARG <- isolate(input$PF_TARG)
        
        validate(
          need(PNC >= PTARG,"- The Probability of Failure given Nonconformance is less than the Target Probability of Failure which produces invalid probability for the IRR (the numerator becomes a negative number making the ratio less than 0)"),
          need(PC <= PTARG,"- The Probability of Failure given Conformance is greater than the Target Probability of Failure which produces invalid probability for the IRR (the denominator is less than the numerator, making the ratio greater than 1)"),
          need(PC != PNC,"- The Probability of Failure given Conformance is equal to Probability of Failure given Nononformance which produces invalid probability for the IRR (the denominator goes to zero violating a fundamental mathematical principle)")
        )
        
        IRRVal <- (PNC-PTARG)/(PNC-PC)
        
      }
    })
    
    ## Create Results Display
    
    # output value
    
    output$selectedVar <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        tags$div(round(analysis_results_1(),decimalplaces(input$PF_TARG)),class = "number-box")  # Replace "42" with a dynamic value
        
      }  
    })
    
    # results comment
    
    output$results_comment_s1 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[1])
        )
        
        # Return all three div elements
        tagList(div1)
      }
      
    })  
    
    ## Create Discussion
    
    # Uncertainty statements
    
    output$uncertainty_statements_s1 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[1])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[2])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[3])
        )
        
        # Return all three div elements
        tagList(div1, div2, div3)
      }
    })  
    
    # summary comment
    
    output$summary_comment_s1 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[1])
        )
        
        # Return all div elements
        tagList(div1)
      }
    })  
    
    output$recommendation_statement_s1 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          p(recommendation_list[1])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })  
    
    ## End of System 1             ##
    
    ## System 2                    ##
    ## PNC = Point                 ##
    ## PC = Point                  ##
    ## PTARG = Epistemic           ##
    
    ## Create subtitle
    
    output$subtitle_s2 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is a ",type_list[1], ", PC is a ", type_list[1], ", Target is an ",type_list[2]))
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    })  
    
    ## Create data
    
    analysis_results_2 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        # Isolate Variables
        PNC <- isolate(input$PF_NC)    
        PC <- isolate(input$PF_C)
        PTARG_lower <- isolate(input$PF_TARG_lower)
        PTARG_upper <- isolate(input$PF_TARG_upper)
        PTARG <- c(PTARG_lower,PTARG_upper)
        
        #Validate Constraints
        
        validate(
          need(PNC >= PTARG_upper,"- The Probability of Failure given Nonconformance is less than the upper Target Probability of Failure which produces invalid probability for the IRR (the numerator becomes a negative number making the ratio less than 0)"),
          need(PC <= PTARG_lower,"- The Probability of Failure given Conformance is greater than the lower boundary for the Target Probability of Failure which produces invalid probability for the IRR (the denominator is less than the numerator, making the ratio greater than 1)"),
          need(PC != PNC,"- The Probability of Failure given Conformance is equal to Probability of Failure given Nononformance which produces invalid probability for the IRR (the denominator goes to zero violating a fundamental mathematical principle)")
        )
        
        # Create dataframe
        epi_PoF_Table <- expand.grid(PNC,PC,PTARG)
        names(epi_PoF_Table) <- c("PNC", "PC", "Target")
        
        # Add IRR
        epi_PoF_Table$IRR <- (epi_PoF_Table$PNC-epi_PoF_Table$Target)/(epi_PoF_Table$PNC-epi_PoF_Table$PC)
        
        return(epi_PoF_Table)
        
      }
    })
    
    ## Create Results
    
    # build results table
    
    output$epi_PoF_table_s2 <- renderTable({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        analysis_results_2()
      }
      
    },
    digits = 9,
    striped = TRUE,
    caption = "Table of IRR Estimates",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # add results comment
    
    output$results_comment_s2 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[2])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })  
    
    ## Create Summary
    
    # build summary table
    
    output$epi_values_table_s2 <- renderTable({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        min_IRR <- min(analysis_results_2()$IRR)
        max_IRR <- max(analysis_results_2()$IRR)
        
        data.frame(
          Parameter = c("Minimum IRR", "Maximum IRR", "Epistemic Value"),
          Value = c(
            sprintf("%.6f", min_IRR),
            sprintf("%.6f", max_IRR),
            sprintf("%.6f", max_IRR)
          ),
          stringsAsFactors = FALSE
        )
      }
    }, 
    striped = TRUE, 
    hover = TRUE, 
    bordered = TRUE,
    caption = "Summary Table of minimum and maximum IRR Estimates",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # add Uncertainty Comments
    
    output$uncertainty_statements_s2 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[1])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[2])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[6])
        )
        
        # Return all div elements
        
        tagList(div1, div2, div3)
      }
    })  
    
    # summary comment
    
    output$summary_comment_s2 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[2])
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    })  
    
    ## Recommendation
    
    output$recommendation_statement_s2 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          p(recommendation_list[1])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })  
    
    ## End if System 2             ##
    
    ## System 3                    ##
    ## PNC = Point                 ##
    ## PC = Point                  ##
    ## PTARG = Aleatoric           ##
    
    ## Create Subtitle
    
    output$subtitle_s3 <- renderUI({
      
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is a ",type_list[1], ", PC is a ", type_list[1], ", Target is an ",type_list[3]))
        )
        
        # Return all div elements
        tagList(div1)
      }
    }) 
    
    ## Create data
    
    analysis_results_3 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        # Isolate Variables
        
        PNC <- isolate(input$PF_NC)
        PC <- isolate(input$PF_C)
        TARG_dist <- isolate(input$TARG_in)
        
        if(TARG_dist == "TARG_norm"){
          # Normal distribution
          TARG_mean <- isolate(input$PF_TARG_mean)
          TARG_sd <- isolate(input$PF_TARG_sd)
          PoF <- rnorm(100000, mean = TARG_mean, sd = TARG_sd)
        } 
        else if(TARG_dist == "TARG_beta"){
          # Beta Distribution
          TARG_shape1 <- isolate(input$PF_TARG_shape1)
          TARG_shape2 <- isolate(input$PF_TARG_shape2)
          PoF <- rbeta(100000, shape1 = TARG_shape1, shape2 = TARG_shape2)
        }
        else if(TARG_dist == "TARG_lgnorm"){
          #Log Normal
          TARG_lgmean <- isolate(input$PF_TARG_lgMean)
          TARG_lgSD <- isolate(input$PF_TARG_lgSD)
          PoF <- rlnorm(100000, meanlog = TARG_lgMean, sdlog = TARG_lgSD)
        }
        
        #Validate Constraints
        
        validate(
          need(PNC >= PoF,"- The Probability of Failure given Nonconformance is less than the upper Target Probability of Failure which produces invalid probability for the IRR (the numerator becomes a negative number making the ratio less than 0)"),
          need(PC <= PoF,"- The Probability of Failure given Conformance is greater than the lower boundary for the Target Probability of Failure which produces invalid probability for the IRR (the denominator is less than the numerator, making the ratio greater than 1)"),
          need(PC != PNC,"- The Probability of Failure given Conformance is equal to Probability of Failure given Nononformance which produces invalid probability for the IRR (the denominator goes to zero violating a fundamental mathematical principle)")
        )
        
        # Create dataframe
        
        df <- data.frame("PNC" = rep(PNC, times = 100000), "PC" = rep(PC, times = 100000), "PoF"= PoF)
        
        # Add IRR
        
        df$IRR <- round((df$PNC - df$PoF)/(df$PNC - df$PC),digits = 9)
        
        # Check Constraints
        
        df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
        
        df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
        
        return(df)
        
      }
      
    })
    
    ## Create Results Output
    
    # create results histogram
    
    output$aleatoric_hist_s3 <- renderPlotly({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        fig <- plot_ly(x = ~analysis_results_3()$IRR, type = "histogram") %>%
          layout(
            title = "Plot of Estimated IRR Values for Aleatoric Target",
            xaxis = list(title = "IRR Distribution"),
            yaxis = list(title = "Frequency")
          )
        
        fig <- fig %>% config(
          displayModeBar = TRUE,  # Show the toolbar
          modeBarButtonsToRemove = c(
            'select2d',
            'lasso2d',
            'autoScale2d',
            'hoverClosestCartesian',
            'hoverCompareCartesian',
            'toggleSpikelines'
          ),
          displaylogo = FALSE,  # Remove plotly logo
          toImageButtonOptions = list(
            format = 'png',
            filename = 'IRR_plot',
            height = 600,
            width = 800,
            scale = 1
          )
        )
        
        fig
        
      }
    })
    
    # create measure of utilization
    
    output$count_data_s3 <- renderPrint({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        value <- nrow(analysis_results_3())/100000
        paste("Fraction of simulated values included in the histogram", value)
        
      }
    })
    
    # add results comment
    
    output$results_comment_s3 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[3])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Discussion
    
    # create summarization table
    
    output$quantile_data_s3 <- renderTable({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        quantiles <- round(quantile(analysis_results_3()$IRR, probs = c(0.95, 0.975, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999, 0.99999999, 0.999999999)),digits = 9)
        quantile_table <- (data.frame(
          Quantile = names(quantiles),
          Value = as.numeric(quantiles)
        ))
      }
    }, digits = 9, 
    caption = "Maximum IRR Values",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # create 95% interval
    
    output$interval_data_s3 <- renderTable({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        quantiles <- round(quantile(analysis_results_3()$IRR, probs = c(0.025, 0.975)),digits = 9)
        quantile_table <- (data.frame(
          Quantile = names(quantiles),
          Value = as.numeric(quantiles)
        ))
        
      }
    }, digits = 9, caption = "95% Interval",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    # create ecdf plot
    
    output$ecdf_plot_s3 <- renderPlot({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        fig <- ggplot(analysis_results_3(),aes(x = IRR))+
          stat_ecdf() +
          ggtitle("ECDF for IRR from Simulation (Cumulative Density Function)")
        fig
        
      }
    })
    
    # create uncertainty statements
    
    output$uncertainty_statements_s3 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[1])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[2])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #f0f8ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #4CAF50;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[9])
        )
        
        # Return all three div elements
        tagList(div1, div2, div3)
      }
    })  
    
    # add summary comment
    
    output$summary_comment_s3 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[3])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Recommendations
    
    output$recommendation_statement_s3 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          p(recommendation_list[1])
        )
        
        tagList(div1)
      }
    })  
    
    ## End System 3                                   ##                               
    
    ## System 4                                       ##
    ## PNC = Point                 ##
    ## PC = Epistemic              ##
    ## PTARG = Point               ##
    
    ## Create Subtitle
    
    output$subtitle_s4 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is a ",type_list[1], ", PC is an ", type_list[2], ", Target is a ",type_list[1]))
        )
        
        # Return all div elements
        tagList(div1)
      }
    }) 
    
    ## create data
    
    analysis_results_4 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        # Isolate Variables
        PNC <- isolate(input$PF_NC)    
        PC_lower <- isolate(input$PF_C_lower)
        PC_upper <- isolate(input$PF_C_upper)
        PC <- c(PC_lower,PC_upper)
        PTARG <- isolate(input$PF_TARG)
        
        #Validate Constraints
        
        validate(
          need(PNC >= PTARG,"- The Probability of Failure given Nonconformance is less than the Target Probability of Failure which produces invalid probability for the IRR (the numerator becomes a negative number making the ratio less than 0)"),
          need(PC_upper <= PTARG,"- The Probability of Failure given Conformance upper boundary is greater than the Target Probability of Failure which produces invalid probability for the IRR (the denominator is less than the numerator, making the ratio greater than 1)"),
          need(PC_lower < PC_upper,"- The Probability of Failure given Conformance lower boundary is greater than the the Probability of Failure given Conformance upper boundary" ),
          need(PC_lower != PNC,"- The Probability of Failure given Conformance lower boundary is equal to Probability of Failure given Nononformance which produces invalid probability for the IRR (the denominator goes to zero violating a fundamental mathematical principle)"),
          need(PC_upper != PNC,"- The Probability of Failure given Conformance upper boundary is equal to Probability of Failure given Nononformance which produces invalid probability for the IRR (the denominator goes to zero violating a fundamental mathematical principle)")
        )
        
        # Create dataframe
        
        epi_PoF_Table <- expand.grid(PNC,PC,PTARG)
        names(epi_PoF_Table) <- c("PNC", "PC", "Target")
        
        # Add IRR
        epi_PoF_Table$IRR <- (epi_PoF_Table$PNC-epi_PoF_Table$Target)/(epi_PoF_Table$PNC-epi_PoF_Table$PC)    
        
        return(epi_PoF_Table)
        
      }
    })
    
    ## Create Results
    
    # build Results table
    
    output$epi_PoF_table_s4 <- renderTable({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        analysis_results_4()
        
      }
    },
    digits = 9,
    striped = TRUE,
    caption = "Table of IRR Estimates",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # add results comment
    
    output$results_comment_s4 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[4])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Summary 
    
    # build summary table
    
    output$epi_values_table_s4 <- renderTable({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        min_IRR <- min(analysis_results_4()$IRR)
        max_IRR <- max(analysis_results_4()$IRR)
        
        data.frame(
          Parameter = c("Minimum IRR", "Maximum IRR", "Epistemic Value"),
          Value = c(
            sprintf("%.6f", min_IRR),
            sprintf("%.6f", max_IRR),
            sprintf("%.6f", max_IRR)
          ),
          stringsAsFactors = FALSE
        )
        
      }
    }, 
    striped = TRUE, 
    hover = TRUE, 
    bordered = TRUE,
    caption = "Summary Table of minimum and maximum IRR Estimates",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # create uncertainty statements
    
    output$uncertainty_statements_s4 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[1])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[5])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[3])
        )
        
        # Return all three div elements
        tagList(div1, div2, div3)
      }
    })  
    
    # add summary comment
    
    output$summary_comment_s4 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[4])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Recommendations
    
    output$recommendation_statement_s4 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          p(recommendation_list[1])
        )
        
        tagList(div1)
      }
    })  
    
    ## End System 4                                   ##
    
    ## System 5                                       ##
    ## PNC = Point                 ##
    ## PC = Epistemic              ##
    ## PTARG = Epistemic           ##
    
    ## Create Subtitle
    
    output$subtitle_s5 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is a ",type_list[1], ", PC is an ", type_list[2], ", Target is an ",type_list[2]))
        )
        
        # Return all div elements
        tagList(div1)
      }
    }) 
    
    ## Create data
    
    analysis_results_5 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        # Isolate Variables
        
        PNC <- isolate(input$PF_NC)
        PC_lower <- isolate(input$PF_C_lower)
        PC_upper <- isolate(input$PF_C_upper)
        PC <- c(PC_lower,PC_upper)
        PTARG_lower <- isolate(input$PF_TARG_lower)
        PTARG_upper <- isolate(input$PF_TARG_upper)
        PTARG <- c(PTARG_lower,PTARG_upper)
        
        #Validate Constraints
        
        validate(
          need(PNC >= PTARG_upper,"- The Probability of Failure given Nonconformance is less than the Target Probability of Failure which produces invalid probability for the IRR (the numerator becomes a negative number making the ratio less than 0)"),
          need(PC_upper <= PTARG_lower,"- The Probability of Failure given Conformance is greater than the Target Probability of Failure which produces invalid probability for the IRR (the denominator is less than the numerator, making the ratio greater than 1)"),
          need(PC_lower < PC_upper,"- The Probability of Failure given Conformance lower is greater than the upper boundary for the Probability of Failure given Conformance)" ),
          need(PC_lower != PNC,"- The Probability of Failure given Conformance lower boundary is equal to Probability of Failure given Nononformance which produces invalid probability for the IRR (the denominator goes to zero violating a fundamental mathematical principle)"),
          need(PC_upper != PNC,"- The Probability of Failure given Conformance upper boundary is equal to Probability of Failure given Nononformance which produces invalid probability for the IRR (the denominator goes to zero violating a fundamental mathematical principle)")
        )
        
        # Create dataframe
        
        epi_PoF_Table <- expand.grid(PNC,PC,PTARG)
        names(epi_PoF_Table) <- c("PNC", "PC", "Target")
        
        #Add IRR
        epi_PoF_Table$IRR <- (epi_PoF_Table$PNC-epi_PoF_Table$Target)/(epi_PoF_Table$PNC-epi_PoF_Table$PC)
        
        return(epi_PoF_Table)
        
      }
    })
    
    # build results table
    
    output$epi_PoF_table_s5 <- renderTable({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        analysis_results_5()
        
      }
    },
    digits = 9,
    striped = TRUE,
    caption = "Table of IRR Estimates",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # add results comment
    
    output$results_comment_s5 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[5])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Summary 
    
    # build summary table
    
    output$epi_values_table_s5 <- renderTable({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        min_IRR <- min(analysis_results_5()$IRR)
        max_IRR <- max(analysis_results_5()$IRR)
        
        data.frame(
          Parameter = c("Minimum IRR", "Maximum IRR", "Epistemic Value"),
          Value = c(
            sprintf("%.6f", min_IRR),
            sprintf("%.6f", max_IRR),
            sprintf("%.6f", max_IRR)
          ),
          stringsAsFactors = FALSE
        )
        
      }
    }, 
    striped = TRUE, 
    hover = TRUE, 
    bordered = TRUE,
    caption = "Summary Table of minimum and maximum IRR Estimates",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # create uncertainty statements
    
    output$uncertainty_statements_s5 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[1])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[5])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[9])
        )
        
        # Return all div elements
        
        tagList(div1, div2, div3)
      }
    })  
    
    # add summary comment
    
    output$summary_comment_s5 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[5])
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    })
    
    ## Create Recommendations
    
    output$recommendation_statement_s5 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          p(recommendation_list[1])
        )
        
        tagList(div1)
      }
    })  
    
    ## End System 5                ##
    
    ## System 6                    ##
    ## PNC = Point                 ##
    ## PC = Epistemic              ##
    ## PTARG = Aleatoric           ##
    
    ## Create Subtitle
    
    output$subtitle_s6 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is a ",type_list[1], ", PC is an ", type_list[2], ", Target is an ",type_list[3]))
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    }) 
    
    ## Create data
    
    analysis_results_6 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        PNC <- isolate(input$PF_NC)
        PC_lower <- isolate(input$PF_C_lower)
        PC_upper <- isolate(input$PF_C_upper)
        PC <- c(PC_lower,PC_upper)
        
        if(input$TARG_in == "TARG_norm"){
          # Normal distribution
          PTARG_mean <- isolate(input$PF_TARG_mean)
          PTARG_sd <- isolate(input$PF_TARG_sd)
          PoF <- rnorm(100000, mean = PTARG_mean, sd = PTARG_sd)
        } 
        else if(input$TARG_in == "TARG_beta"){
          PTARG_shape1 <- isolate(input$PF_TARG_shape1)
          PTARG_shape2 <- isolate(input$PF_TARG_shape2)
          PoF <- rbeta(100000, shape1 = PTARG_shape1, shape2 = PTARG_shape2)
        }
        else if(input$TARG_in == "TARG_lgnorm"){
          PTARG_lgMmean <- isolate(input$PF_TARG_lgMean)
          PTARG_lgSD <- isolate(input$PF_TARG_lgSD)
          PoF <- rlnorm(100000, meanlog = PTARG_lgMean, sdlog = PTARG_lgSD)
        }
        
        df <- data.frame("PNC" = rep(PNC,times = 100000), "PC" = rep(PC, times = 50000), "PoF" = PoF)
        
        df$IRR <- round((PNC - PoF)/(PNC - PC),digits = 9)
        
        df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
        
        df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
        
        return(df)
        
      }
    })
    
    ## Create Results
    
    # create results histogram
    
    output$aleatoric_hist_s6 <- renderPlotly({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        fig <- plot_ly(type = "histogram", alpha = 0.8) 
        
        fig <- fig %>% add_histogram(x = ~analysis_results_6()[analysis_results_6()$PC == input$PF_C_lower,"IRR"], name = "PC lower")
        
        fig <- fig %>% add_histogram(x = ~analysis_results_6()[analysis_results_6()$PC == input$PF_C_upper,"IRR"], name = "PC upper")
        
        fig <- fig %>%
          layout(
            xaxis = list(title = "IRR Distribution"),
            yaxis = list(title = "Frequency",
                         barmode = "overlay")
          )
        
        fig <- fig %>% config(
          displayModeBar = TRUE,  # Show the toolbar
          modeBarButtonsToRemove = c(
            'select2d',
            'lasso2d',
            'autoScale2d',
            'hoverClosestCartesian',
            'hoverCompareCartesian',
            'toggleSpikelines'
          ),
          displaylogo = FALSE,  # Remove plotly logo
          toImageButtonOptions = list(
            format = 'png',
            filename = 'IRR_plot',
            height = 600,
            width = 800,
            scale = 1
          )
        )
        
        fig
        
      }
    })
    
    # create measure of utilization
    
    output$count_data_s6 <- renderPrint({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        value <- nrow(analysis_results_6())/100000
        paste("Fraction of simulated values included in the histogram", value)
        
      }
    })
    
    # add results comment
    
    output$results_comment_s6 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[6])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    
    ## Summary
    
    # create summarization table
    
    output$quantile_data_s6 <- renderTable({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        quantiles <- round(quantile(analysis_results_6()$IRR, probs = c(0.95, 0.975, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999, 0.99999999, 0.999999999)),digits = 9)
        quantile_table <- (data.frame(
          Quantile = names(quantiles),
          Value = as.numeric(quantiles)
        ))
        
      }
    }, 
    digits = 9,
    caption = "Maximum IRR Values",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # create 95% interval
    
    output$interval_data_s6 <- renderTable({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        dflow <- analysis_results_6()[analysis_results_6()$PC == min(analysis_results_6()$PC),"IRR"]
        dfhigh <- analysis_results_6()[analysis_results_6()$PC == max(analysis_results_6()$PC),"IRR"]
        
        quantile_table <- data.frame("PC" = c(min(analysis_results_6()$PC),max(analysis_results_6()$PC)),
                                     "0.025" = c(quantile(dflow,0.025),quantile(dfhigh,0.025)),
                                     "0.975" = c(quantile(dflow,0.975),quantile(dfhigh,0.975))
        )
        return(quantile_table)
        
      }
    }, 
    digits = 9, 
    caption = "95% Interval",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    # create ecdf plot
    
    output$ecdf_plot_s6 <- renderPlot({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        dflow <- analysis_results_6()[analysis_results_6()$PC == min(analysis_results_6()$PC),"IRR"]
        dfhigh <- analysis_results_6()[analysis_results_6()$PC == max(analysis_results_6()$PC),"IRR"]
        
        fig <-ggplot() +
          stat_ecdf(aes(dflow), color = "darkblue") +
          stat_ecdf(aes(dfhigh), color = "red") +
          
          #Point LL
          geom_segment(aes(x = quantile(dflow,0.025), y = 0.025-0.02, xend = quantile(dflow,0.025), yend = 0.025+0.02)) +
          geom_segment(aes(x = (quantile(dflow,0.025)-0.005), y = 0.025, xend = (quantile(dflow,0.025)+0.005), yend = 0.025)) +
          annotate("text", x=quantile(dflow,0.025)-0.010, y=0.025+0.030, label= round(quantile(dflow,0.025),3)) +
          
          #Point RL
          geom_segment(aes(x = quantile(dfhigh,0.025), y = 0.025-0.02, xend = quantile(dfhigh,0.025), yend = 0.025+0.02)) +
          geom_segment(aes(x = (quantile(dfhigh,0.025)-0.005), y = 0.025, xend = (quantile(dfhigh,0.025)+0.005), yend = 0.025)) +
          annotate("text", x=quantile(dfhigh,0.025)+0.005, y=0.025+0.030, label= round(quantile(dfhigh,0.025),3)) +
          
          # Point LH
          geom_segment(aes(x = quantile(dflow,0.975), y = 0.975-0.02, xend = quantile(dflow,0.975), yend = 0.975+0.02)) +
          geom_segment(aes(x = (quantile(dflow,0.975)-0.005), y = 0.975, xend = (quantile(dflow,0.975)+0.005), yend = 0.975)) +
          annotate("text", x=quantile(dflow,0.975)-0.010, y=0.975-0.030, label= round(quantile(dflow,0.975),3)) +
          
          # Point RL
          geom_segment(aes(x = quantile(dfhigh,0.975), y = 0.975-0.02, xend = quantile(dfhigh,0.975), yend = 0.975+0.02)) +
          geom_segment(aes(x = (quantile(dfhigh,0.975)-0.005), y = 0.975, xend = (quantile(dfhigh,0.975)+0.005), yend = 0.975)) +
          annotate("text", x=quantile(dfhigh,0.975)-0.010, y=0.975-0.030, label= round(quantile(dfhigh,0.975),3)) +
          
          ggtitle("ECDF for IRR from Simulation (Cumulative Density Function)") +
          xlab("IRR") +
          ylab("Probability")
        
        fig
        
      }
    })
    
    # create uncertainty statements
    
    output$uncertainty_statements_s6 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[1])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[2])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #f0f8ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #4CAF50;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[9])
        )
        
        # Return all three div elements
        tagList(div1, div2, div3)
      }
    })  
    
    # add summary comment
    
    output$summary_comment_s6 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[6])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Recommendations
    
    output$recommendation_statement_s6 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          p(recommendation_list[1])
        )
        
        tagList(div1)
      }
    })  
    
    ##                                          ##
    
    ## System 7  ##
    ## PNC = Point                 ##
    ## PC = Aleatoric              ##
    ## PTARG = Point               ##
    
    ## Create Subtitle
    
    output$subtitle_s7 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is a ",type_list[1], ", PC is an ", type_list[3], ", Target is an ",type_list[1]))
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    }) 
    
    ## Create data
    
    analysis_results_7 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        PNC <- isolate(input$PF_NC)
        
        if(input$PC_in == "PC_norm"){
          # Normal distribution
          PC_mean <- isolate(input$PF_C_mean)
          PC_sd <- isolate(input$PF_C_sd)
          PC <- rnorm(100000, mean = PC_mean, sd = PC_sd)
        } 
        else if(input$PC_in == "PC_beta"){
          # Beta distribution
          PC_shape1 <- isolate(input$PF_C_shape1)
          PC_shape2 <- isolate(input$PF_C_shape2)
          PC <- rbeta(100000, shape1 = PC_shape1, shape2 = PC_shape2)
        }
        else if(input$PC_in == "PC_lgnorm"){
          # Log Normal distribution
          PC_lgMean <- isolate(input$PF_C_lgMean)
          PC_lgSD <- isolate(input$PF_C_lgSD)
          PC <- rlnorm(100000, meanlog = PC_lgMean, sdlog = PC_lgSD)
        }
        
        PoF <- isolate(input$PF_TARG)
        
        df <- data.frame("PNC" = rep(PNC, times = 100000), "PC" = PC, "PoF" = rep(PoF, times = 100000))
        
        df$IRR <- round((PNC - PoF)/(PNC - PC),digits = 9)
        
        df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
        
        df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
        
        return(df)
        
      }
    })
    
    ## Create Results
    
    # create results histogram
    
    output$aleatoric_hist_s7 <- renderPlotly({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        fig <- plot_ly(type = "histogram", alpha = 0.8) 
        
        fig <- fig %>% add_histogram(x = ~analysis_results_7()$IRR)
        
        fig <- fig %>%
          layout(
            xaxis = list(title = "IRR Distribution"),
            yaxis = list(title = "Frequency")
          )
        
        fig <- fig %>% config(
          displayModeBar = TRUE,  # Show the toolbar
          modeBarButtonsToRemove = c(
            'select2d',
            'lasso2d',
            'autoScale2d',
            'hoverClosestCartesian',
            'hoverCompareCartesian',
            'toggleSpikelines'
          ),
          displaylogo = FALSE,  # Remove plotly logo
          toImageButtonOptions = list(
            format = 'png',
            filename = 'IRR_plot',
            height = 600,
            width = 800,
            scale = 1
          )
        )
        
        fig
        
      }
    })
    
    #create measure of utilization
    
    output$count_data_s7 <- renderPrint({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        value <- nrow(analysis_results_7())/100000
        paste("Fraction of simulated values included in the histogram", value)
        
      }
    })
    
    # add summary comment
    
    output$results_comment_s7 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[7])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Summary
    
    # create summarization table
    
    output$quantile_data_s7 <- renderTable({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        quantiles <- round(quantile(analysis_results_7()$IRR, probs = c(0.95, 0.975, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999, 0.99999999, 0.999999999)),digits = 9)
        quantile_table <- (data.frame(
          Quantile = names(quantiles),
          Value = as.numeric(quantiles)
        ))
        
      }
    }, digits = 9, 
    caption = "Maximum IRR Values",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # create 95% interval
    
    output$interval_data_s7 <- renderTable({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        quantiles <- round(quantile(analysis_results_7()$IRR, probs = c(0.025, 0.975)),digits = 9)
        quantile_table <- (data.frame(
          Quantile = names(quantiles),
          Value = as.numeric(quantiles)
        ))
        
      }
    }, digits = 9, caption = "95% Interval",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    # create ecdf plot
    
    output$ecdf_plot_s7 <- renderPlot({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        fig <- ggplot(analysis_results_7(),aes(x = IRR))+
          stat_ecdf() +
          ggtitle("ECDF for IRR from Simulation (Cumulative Density Function)")
        fig
        
      }
    })
    
    # create uncertainty statements
    
    output$uncertainty_statements_s7 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[1])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #f0f8ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #4CAF50;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[8])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[3])
        )
        
        # Return all three div elements
        tagList(div1, div2, div3)
      }
    })  
    
    # add summary comment
    
    output$summary_comment_s7 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[7])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Recommendations
    
    output$recommendation_statement_s7 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          p(recommendation_list[1])
        )
        
        tagList(div1)
      }
    })  
    
    ## End System 7                                  ##                               
    
    ## System 8  ##
    ## PNC = Point                 ##
    ## PC = Aleatoric              ##
    ## PTARG = Epistemic           ##
    
    ## Create Subtitle
    
    output$subtitle_s8 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is a ",type_list[1], ", PC is an ", type_list[3], ", Target is an ",type_list[2]))
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    }) 
    
    ## Create data
    
    analysis_results_8 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        PNC <- isolate(input$PF_NC)
        
        if(input$PC_in == "PC_norm"){
          # Normal distribution
          PC_mean <- isolate(input$PF_C_mean)
          PC_sd <- isolate(input$PF_C_sd)
          PC <- rnorm(100000, mean = PC_mean, sd = PC_sd)
        } 
        else if(input$PC_in == "PC_beta"){
          # Beta distribution
          PC_shape1 <- isolate(input$PF_C_shape1)
          PC_shape2 <- isolate(input$PF_C_shape2)
          PC <- rbeta(100000, shape1 = PC_shape1, shape2 = PC_shape2)
        }
        else if(input$PC_in == "PC_lgnorm"){
          # Log Normal distribution
          PC_lgMean <- isolate(input$PF_C_lgMean)
          PC_lgSD <- isolate(input$PF_C_lgSD)
          PC <- rlnorm(100000, meanlog = PC_lgMean, sdlog = PC_lgSD)
        }
        
        PTARG_lower <- isolate(input$PF_TARG_lower)
        PTARG_upper <- isolate(input$PF_TARG_upper)
        PoF <- c(PTARG_lower,PTARG_upper)
        
        df <- data.frame("PNC" = rep(PNC, times = 100000), "PC" = PC, "PoF" = rep(PoF, times = 50000))
        
        df$IRR <- round((df$PNC - df$PoF)/(df$PNC - df$PC),digits = 9)
        
        df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
        
        df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
        
        return(df)
        
      }
    })
    
    ## Create Results
    
    # create results histogram
    
    output$aleatoric_hist_s8 <- renderPlotly({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        fig <- plot_ly(type = "histogram", alpha = 0.8) 
        
        fig <- fig %>% add_histogram(x = ~analysis_results_8()[analysis_results_8()$PoF == input$PF_TARG_lower,"IRR"], name = "PoF lower")
        
        fig <- fig %>% add_histogram(x = ~analysis_results_8()[analysis_results_8()$PoF == input$PF_TARG_upper,"IRR"], name = "PoF upper")
        
        fig <- fig %>%
          layout(
            xaxis = list(title = "IRR Distribution"),
            yaxis = list(title = "Frequency",
                         barmode = "overlay")
          )
        
        fig <- fig %>% config(
          displayModeBar = TRUE,  # Show the toolbar
          modeBarButtonsToRemove = c(
            'select2d',
            'lasso2d',
            'autoScale2d',
            'hoverClosestCartesian',
            'hoverCompareCartesian',
            'toggleSpikelines'
          ),
          displaylogo = FALSE,  # Remove plotly logo
          toImageButtonOptions = list(
            format = 'png',
            filename = 'IRR_plot',
            height = 600,
            width = 800,
            scale = 1
          )
        )
        
        fig
        
      }
    })
    
    # create measure of utilization
    
    output$count_data_s8 <- renderPrint({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        value <- nrow(analysis_results_8())/100000
        paste("Fraction of simulated values included in the histogram", value)
        
      }
    })
    
    # add summary comment
    
    output$results_comment_s8 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[8])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Summary
    
    # create summarization table
    
    output$quantile_data_s8 <- renderTable({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        quantiles <- round(quantile(analysis_results_8()$IRR, probs = c(0.95, 0.975, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999, 0.99999999, 0.999999999)),digits = 9)
        quantile_table <- (data.frame(
          Quantile = names(quantiles),
          Value = as.numeric(quantiles)
        ))
        
      }
    }, 
    digits = 9,
    caption = "Maximum IRR Values",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # create 95% interval
    
    output$interval_data_s8 <- renderTable({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        dflow <- analysis_results_8()[analysis_results_8()$PoF == min(analysis_results_8()$PoF),"IRR"]
        dfhigh <- analysis_results_8()[analysis_results_8()$PoF == max(analysis_results_8()$PoF),"IRR"]
        
        quantile_table <- data.frame("PoF" = c(min(analysis_results_8()$PoF),max(analysis_results_8()$PoF)),
                                     "0.025" = c(quantile(dflow,0.025),quantile(dfhigh,0.025)),
                                     "0.975" = c(quantile(dflow,0.975),quantile(dfhigh,0.975))
        )
        return(quantile_table)
        
      }
    }, 
    digits = 9, 
    caption = "95% Interval",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    # create ecdf plot
    
    output$ecdf_plot_s8 <- renderPlot({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        dflow <- analysis_results_8()[analysis_results_8()$PoF == min(analysis_results_8()$PoF),"IRR"]
        dfhigh <- analysis_results_8()[analysis_results_8()$PoF == max(analysis_results_8()$PoF),"IRR"]
        
        fig <-ggplot() +
          stat_ecdf(aes(dflow), color = "darkblue") +
          stat_ecdf(aes(dfhigh), color = "red") +
          
          #Point LL
          geom_segment(aes(x = quantile(dflow,0.025), y = 0.025-0.02, xend = quantile(dflow,0.025), yend = 0.025+0.02)) +
          geom_segment(aes(x = (quantile(dflow,0.025)-0.005), y = 0.025, xend = (quantile(dflow,0.025)+0.005), yend = 0.025)) +
          annotate("text", x=quantile(dflow,0.025)-0.010, y=0.025+0.030, label= round(quantile(dflow,0.025),3)) +
          
          #Point RL
          geom_segment(aes(x = quantile(dfhigh,0.025), y = 0.025-0.02, xend = quantile(dfhigh,0.025), yend = 0.025+0.02)) +
          geom_segment(aes(x = (quantile(dfhigh,0.025)-0.005), y = 0.025, xend = (quantile(dfhigh,0.025)+0.005), yend = 0.025)) +
          annotate("text", x=quantile(dfhigh,0.025)+0.005, y=0.025+0.030, label= round(quantile(dfhigh,0.025),3)) +
          
          # Point LH
          geom_segment(aes(x = quantile(dflow,0.975), y = 0.975-0.02, xend = quantile(dflow,0.975), yend = 0.975+0.02)) +
          geom_segment(aes(x = (quantile(dflow,0.975)-0.005), y = 0.975, xend = (quantile(dflow,0.975)+0.005), yend = 0.975)) +
          annotate("text", x=quantile(dflow,0.975)-0.010, y=0.975-0.030, label= round(quantile(dflow,0.975),3)) +
          
          # Point RL
          geom_segment(aes(x = quantile(dfhigh,0.975), y = 0.975-0.02, xend = quantile(dfhigh,0.975), yend = 0.975+0.02)) +
          geom_segment(aes(x = (quantile(dfhigh,0.975)-0.005), y = 0.975, xend = (quantile(dfhigh,0.975)+0.005), yend = 0.975)) +
          annotate("text", x=quantile(dfhigh,0.975)-0.010, y=0.975-0.030, label= round(quantile(dfhigh,0.975),3)) +
          
          ggtitle("ECDF for IRR from Simulation (Cumulative Density Function)") +
          xlab("IRR") +
          ylab("Probability")
        
        fig
        
      }
    })
    
    # create uncertainty statements
    
    output$uncertainty_statements_s8 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[1])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #f0f8ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #4CAF50;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[8])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[6])
        )
        
        # Return all three div elements
        tagList(div1, div2, div3)
      }
    })  
    
    # add summary comment
    
    output$summary_comment_s8 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[8])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Recommendations
    
    output$recommendation_statement_s8 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          p(recommendation_list[1])
        )
        
        tagList(div1)
      }
    })  
    
    ## End System 8                ##
    
    ## System 9                    ##
    ## PNC = Point                 ##
    ## PC = Aleatoric              ##
    ## PTARG = Aleatoric           ##
    
    ## Create Subtitle
    
    output$subtitle_s9 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is a ",type_list[1], ", PC is an ", type_list[3], ", Target is an ",type_list[2]))
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    }) 
    
    ## Create data
    
    analysis_results_9 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        # Create Vectors
        
        PNC <- isolate(input$PF_NC)
        
        if(input$PC_in == "PC_norm"){
          # Normal distribution
          PC_mean <- isolate(input$PF_C_mean)
          PC_sd <- isolate(input$PF_C_sd)
          PC <- rnorm(100000, mean = PC_mean, sd = PC_sd)
        } 
        else if(input$PC_in == "PC_beta"){
          # Beta distribution
          PC_shape1 <- isolate(input$PF_C_shape1)
          PC_shape2 <- isolate(input$PF_C_shape2)
          PC <- rbeta(100000, shape1 = PC_shape1, shape2 = PC_shape2)
        }
        else if(input$PC_in == "PC_lgnorm"){
          # Log Normal distribution
          PC_lgMean <- isolate(input$PF_C_lgMean)
          PC_lgSD <- isolate(input$PF_C_lgSD)
          PC <- rlnorm(100000, meanlog = PC_lgMean, sdlog = PC_lgSD)
        }
        
        if(input$TARG_in == "TARG_norm"){
          # Normal distribution
          PTARG_mean <- isolate(input$PF_TARG_mean)
          PTARG_sd <- isolate(input$PF_TARG_sd)
          PoF <- rnorm(100000, mean = PTARG_mean, sd = PTARG_sd)
        } 
        else if(input$TARG_in == "TARG_beta"){
          PTARG_shape1 <- isolate(input$PF_TARG_shape1)
          PTARG_shape2 <- isolate(input$PF_TARG_shape2)
          PoF <- rbeta(100000, shape1 = PTARG_shape1, shape2 = PTARG_shape2)
        }
        else if(input$TARG_in == "TARG_lgnorm"){
          PTARG_lgMmean <- isolate(input$PF_TARG_lgMean)
          PTARG_lgSD <- isolate(input$PF_TARG_lgSD)
          PoF <- rlnorm(100000, meanlog = PTARG_lgMean, sdlog = PTARG_lgSD)
        }
        
        # Create dataframe
        
        df <- data.frame("PNC" = rep(input$PF_NC, times = 100000), "PC" = PC, "PoF" = PoF)
        
        # Add IRR
        
        df$IRR <- round((df$PNC - df$PoF)/(df$PNC - df$PC),digits = 9)
        
        # Check Constraints
        
        df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
        
        df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
        
        return(df)
        
      }
    })
    
    ## Create Results
    
    # create results histogram
    
    output$aleatoric_hist_s9 <- renderPlotly({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        fig <- plot_ly(type = "histogram", alpha = 0.8) 
        
        fig <- fig %>% add_histogram(x = ~analysis_results_9()$IRR)
        
        fig <- fig %>%
          layout(
            xaxis = list(title = "IRR Distribution"),
            yaxis = list(title = "Frequency")
          )
        
        fig <- fig %>% config(
          displayModeBar = TRUE,  # Show the toolbar
          modeBarButtonsToRemove = c(
            'select2d',
            'lasso2d',
            'autoScale2d',
            'hoverClosestCartesian',
            'hoverCompareCartesian',
            'toggleSpikelines'
          ),
          displaylogo = FALSE,  # Remove plotly logo
          toImageButtonOptions = list(
            format = 'png',
            filename = 'IRR_plot',
            height = 600,
            width = 800,
            scale = 1
          )
        )
        
        fig
        
      }
    })
    
    #create measure of utilization
    
    output$count_data_s9 <- renderPrint({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        value <- nrow(analysis_results_9())/100000
        paste("Fraction of simulated values included in the histogram", value)
        
      }
    })
    
    # add summary comment
    
    output$results_comment_s9 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[9])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Summary
    
    # create summarization table
    
    output$quantile_data_s9 <- renderTable({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        quantiles <- round(quantile(analysis_results_9()$IRR, probs = c(0.95, 0.975, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999, 0.99999999, 0.999999999)),digits = 9)
        quantile_table <- (data.frame(
          Quantile = names(quantiles),
          Value = as.numeric(quantiles)
        ))
        
      }
    }, digits = 9, 
    caption = "Maximum IRR Values",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # create 95% interval
    
    output$interval_data_s9 <- renderTable({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        quantiles <- round(quantile(analysis_results_9()$IRR, probs = c(0.025, 0.975)),digits = 9)
        quantile_table <- (data.frame(
          Quantile = names(quantiles),
          Value = as.numeric(quantiles)
        ))
        
      }
    }, digits = 9, caption = "95% Interval",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    # create ecdf plot
    
    output$ecdf_plot_s9 <- renderPlot({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        fig <- ggplot(analysis_results_9(),aes(x = IRR))+
          stat_ecdf() +
          ggtitle("ECDF for IRR from Simulation (Cumulative Density Function)")
        fig
        
      }
    })
    
    # create uncertainty statements
    
    output$uncertainty_statements_s9 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[1])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #f0f8ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #4CAF50;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[8])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #f0f8ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #4CAF50;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[3])
        )
        
        # Return all three div elements
        tagList(div1, div2, div3)
      }
    })  
    
    # add summary comment
    
    output$summary_comment_s9 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[9])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Recommendations
    
    output$recommendation_statement_s9 <- renderUI({
      
      if(input$eType_PNC == 'unknownUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          p(recommendation_list[1])
        )
        
        tagList(div1)
      }
    })  
    
    ## End System 9                                 ##
    
    ## System 10                                    ##
    ## PNC = Point                 ##
    ## PC = Aleatoric              ##
    ## PTARG = Aleatoric           ##
    
    ## Create Subtitle
    
    output$subtitle_s10 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is an ",type_list[2], ", PC is a ", type_list[1], ", Target is a ",type_list[1]))
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    }) 
    
    ## Create data
    
    analysis_results_10 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        # Create Vectors
        
        PNC_lower <- isolate(input$PF_NC_lower)
        PNC_upper <- isolate(input$PF_NC_upper)
        PNC <- c(PNC_lower,PNC_upper)
        PC <- isolate(input$PF_C)
        PoF <- isolate(input$PF_TARG)
        
        # Validate Constraints
        
        validate(
          need(PNC >= PoF,"- The Probability of Failure given Nonconformance is less than the upper Target Probability of Failure which produces invalid probability for the IRR (the numerator becomes a negative number making the ratio less than 0)"),
          need(PC <= PoF,"- The Probability of Failure given Conformance is greater than the lower boundary for the Target Probability of Failure which produces invalid probability for the IRR (the denominator is less than the numerator, making the ratio greater than 1)"),
          need(PC != PNC,"- The Probability of Failure given Conformance is equal to Probability of Failure given Nononformance which produces invalid probability for the IRR (the denominator goes to zero violating a fundamental mathematical principle)")
        )
        
        # Create dataframe
        
        df <- expand.grid(PNC,PC,PoF)
        names(df) <- c("PNC", "PC", "PoF")
        
        # Add IRR
        
        df$IRR <- (df$PNC-df$PoF)/(df$PNC-df$PC)
        
        return(df)
        
      }
    })
    
    ## Create Results
    
    # build results table
    
    output$epi_PoF_table_s10 <- renderTable({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        analysis_results_10()
      }
      
    },
    digits = 9,
    striped = TRUE,
    caption = "Table of IRR Estimates",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # add results comment
    
    output$results_comment_s10 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[10])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })  
    
    ## Create Summary
    
    # build summary table
    
    output$epi_values_table_s10 <- renderTable({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        min_IRR <- min(analysis_results_10()$IRR)
        max_IRR <- max(analysis_results_10()$IRR)
        
        data.frame(
          Parameter = c("Minimum IRR", "Maximum IRR", "Epistemic Value"),
          Value = c(
            sprintf("%.6f", min_IRR),
            sprintf("%.6f", max_IRR),
            sprintf("%.6f", max_IRR)
          ),
          stringsAsFactors = FALSE
        )
      }
    }, 
    striped = TRUE, 
    hover = TRUE, 
    bordered = TRUE,
    caption = "Summary Table of minimum and maximum IRR Estimates",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # add Uncertainty Comments
    
    output$uncertainty_statements_s10 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[4])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[2])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[3])
        )
        
        # Return all div elements
        
        tagList(div1, div2, div3)
      }
    })  
    
    # summary comment
    
    output$summary_comment_s10 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[10])
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    })  
    
    ## Recommendation
    
    output$recommendation_statement_s10 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          p(recommendation_list[1])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })  
    
    ## End if System 10             ##
    
    ## System 11                                         ##
    ## PNC = Point                 ##
    ## PC = Aleatoric              ##
    ## PTARG = Aleatoric           ##
    
    ## Create Subtitle
    
    output$subtitle_s11 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is an ",type_list[2], ", PC is a ", type_list[1], ", Target is a ",type_list[2]))
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    }) 
    
    ## Create data
    
    analysis_results_11 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        # Create Vectors
        
        PNC_lower <- isolate(input$PF_NC_lower)
        PNC_upper <- isolate(input$PF_NC_upper)
        PNC <- c(PNC_lower,PNC_upper)
        PC <- isolate(input$PF_C)
        PoF_lower <- isolate(input$PF_TARG_lower)
        PoF_upper <- isolate(input$PF_TARG_upper)
        PoF <- c(input$PF_TARG_lower,input$PF_TARG_upper)
        
        # Validate Constraints
        
        validate(
          need(PNC_lower >= PoF_upper,"- The Probability of Failure given Nonconformance lower boundary is less than the Target Probability of Failure upper boundary which produces invalid probability for the IRR (the numerator becomes a negative number making the ratio less than 0)"),
          need(PC <= PoF_lower,"- The Probability of Failure given Conformance is greater than the Target Probability of Failure lower boundary which produces invalid probability for the IRR (the denominator is less than the numerator, making the ratio greater than 1)"),
          need(PoF_lower < PoF_upper,"- The Target Probability of Failure lower boundary is greater than the Target Probability of Failure upper boundary" ),
          need(PNC_lower < PNC_upper,"- The Probability of Failure given Nononformance lower boundary is greater than the the Probability of Failure given Nonconformance upper boundary" ),
          need(PNC_lower != PC,"- The Probability of Failure given Nonconformance lower boundary is equal to Probability of Failure given Conformance which produces invalid probability for the IRR (the denominator goes to zero violating a fundamental mathematical principle)"),
          need(PNC_upper != PC,"- The Probability of Failure given Nonconformance upper boundary is equal to Probability of Failure given Conformance which produces invalid probability for the IRR (the denominator goes to zero violating a fundamental mathematical principle)")
        )
        
        # Create dataframe
        
        df <- expand.grid(PNC,PC,PoF)
        names(df) <- c("PNC", "PC", "Target")
        
        # Add IRR
        
        df$IRR <- (df$PNC-df$Target)/(df$PNC-df$PC)
        
        return(df)
        
      }
    })
    
    # build table
    
    output$epi_PoF_table_s11 <- renderTable({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        analysis_results_11()
        
      }
    },
    digits = 9,
    striped = TRUE,
    caption = "Table of IRR Estimates",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # add results comment
    
    output$results_comment_s11 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[11])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })  
    
    ## Create Summary
    
    # build summary table
    
    output$epi_values_table_s11 <- renderTable({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        min_IRR <- min(analysis_results_11()$IRR)
        max_IRR <- max(analysis_results_11()$IRR)
        
        data.frame(
          Parameter = c("Minimum IRR", "Maximum IRR", "Epistemic Value"),
          Value = c(
            sprintf("%.6f", min_IRR),
            sprintf("%.6f", max_IRR),
            sprintf("%.6f", max_IRR)
          ),
          stringsAsFactors = FALSE
        )
      }
    }, 
    striped = TRUE, 
    hover = TRUE, 
    bordered = TRUE,
    caption = "Summary Table of minimum and maximum IRR Estimates",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # add Uncertainty Comments
    
    output$uncertainty_statements_s11 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[4])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[2])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[6])
        )
        
        # Return all div elements
        
        tagList(div1, div2, div3)
      }
    })  
    
    # summary comment
    
    output$summary_comment_s11 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[11])
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    })  
    
    ## Recommendation
    
    output$recommendation_statement_s11 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          p(recommendation_list[1])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })  
    
    ## End if System 11             ##
    
    ## System 12                    ##
    ## PNC = Epistemic              ##
    ## PC = Point                   ##
    ## PTARG = Aleatoric            ##
    
    ## Create Subtitle
    
    output$subtitle_s12 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is an ",type_list[2], ", PC is a ", type_list[1], ", Target is a ",type_list[3]))
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    }) 
    
    ## Create data
    
    analysis_results_12 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        PNC_lower <- isolate(input$PF_NC_lower)
        PNC_upper <- isolate(input$PF_NC_upper)
        PNC <- c(PNC_lower, PNC_upper)
        PC <- isolate(input$PF_C)
        
        if(input$TARG_in == "TARG_norm"){
          # Normal distribution
          PTARG_mean <- isolate(input$PF_TARG_mean)
          PTARG_sd <- isolate(input$PF_TARG_sd)
          PoF <- rnorm(100000, mean = PTARG_mean, sd = PTARG_sd)
        } 
        else if(input$TARG_in == "TARG_beta"){
          PTARG_shape1 <- isolate(input$PF_TARG_shape1)
          PTARG_shape2 <- isolate(input$PF_TARG_shape2)
          PoF <- rbeta(100000, shape1 = PTARG_shape1, shape2 = PTARG_shape2)
        }
        else if(input$TARG_in == "TARG_lgnorm"){
          PTARG_lgMmean <- isolate(input$PF_TARG_lgMean)
          PTARG_lgSD <- isolate(input$PF_TARG_lgSD)
          PoF <- rlnorm(100000, meanlog = PTARG_lgMean, sdlog = PTARG_lgSD)
        }
        
        # Create dataframe
        
        df <- data.frame("PNC" = rep(PNC, times = 50000), "PC" = rep(PC, times = 100000), "PoF" = PoF)
        
        # Add IRR
        
        df$IRR <- round((df$PNC - df$PoF)/(df$PNC - df$PC),digits = 9)
        
        # Check Constraints
        
        df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
        
        df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
        
        return(df)
        
      }
    })
    
    # create results histogram
    
    output$aleatoric_hist_s12 <- renderPlotly({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        fig <- plot_ly(type = "histogram", alpha = 1.0) 
        
        fig <- fig %>% add_histogram(x = ~analysis_results_12()[analysis_results_12()$PNC == input$PF_NC_lower,"IRR"], name = "PNC lower")
        
        fig <- fig %>% add_histogram(x = ~analysis_results_12()[analysis_results_12()$PNC == input$PF_NC_upper,"IRR"], name = "PNC upper")
        
        fig <- fig %>%
          layout(
            xaxis = list(title = "IRR Distribution"),
            yaxis = list(title = "Frequency",
                         barmode = "overlay")
          )
        
        fig <- fig %>% config(
          displayModeBar = TRUE,  # Show the toolbar
          modeBarButtonsToRemove = c(
            'select2d',
            'lasso2d',
            'autoScale2d',
            'hoverClosestCartesian',
            'hoverCompareCartesian',
            'toggleSpikelines'
          ),
          displaylogo = FALSE,  # Remove plotly logo
          toImageButtonOptions = list(
            format = 'png',
            filename = 'IRR_plot',
            height = 600,
            width = 800,
            scale = 1
          )
        )
        
        fig
        
      }
    })
    
    #create measure of utilization
    
    output$count_data_s12 <- renderPrint({
      value <- nrow(analysis_results_12())/100000
      paste("Fraction of simulated values included in the histogram", value)
    })
    
    # add results comment
    
    output$results_comment_s12 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[12])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Summary
    
    # create summarization table
    
    output$quantile_data_s12 <- renderTable({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        quantiles <- round(quantile(analysis_results_12()$IRR, probs = c(0.95, 0.975, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999, 0.99999999, 0.999999999)),digits = 9)
        quantile_table <- (data.frame(
          Quantile = names(quantiles),
          Value = as.numeric(quantiles)
        ))
        
      }
    }, 
    digits = 9,
    caption = "Maximum IRR Values",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # create 95% interval
    
    output$interval_data_s12 <- renderTable({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        dflow <- analysis_results_12()[analysis_results_12()$PNC == min(analysis_results_12()$PNC),"IRR"]
        dfhigh <- analysis_results_12()[analysis_results_12()$PNC == max(analysis_results_12()$PNC),"IRR"]
        
        quantile_table <- data.frame("PNC" = c(min(analysis_results_12()$PNC),max(analysis_results_12()$PNC)),
                                     "0.025" = c(quantile(dflow,0.025),quantile(dfhigh,0.025)),
                                     "0.975" = c(quantile(dflow,0.975),quantile(dfhigh,0.975))
        )
        return(quantile_table)
        
      }
    }, 
    digits = 9, 
    caption = "95% Interval",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    # create ecdf plot
    
    output$ecdf_plot_s12 <- renderPlot({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        dflow <- analysis_results_12()[analysis_results_12()$PNC == min(analysis_results_12()$PNC),"IRR"]
        dfhigh <- analysis_results_12()[analysis_results_12()$PNC == max(analysis_results_12()$PNC),"IRR"]
        
        fig <-ggplot() +
          stat_ecdf(aes(dflow), color = "darkblue") +
          stat_ecdf(aes(dfhigh), color = "red") +
          
          #Point LL
          geom_segment(aes(x = quantile(dflow,0.025), y = 0.025-0.02, xend = quantile(dflow,0.025), yend = 0.025+0.02)) +
          geom_segment(aes(x = (quantile(dflow,0.025)-0.005), y = 0.025, xend = (quantile(dflow,0.025)+0.005), yend = 0.025)) +
          annotate("text", x=quantile(dflow,0.025)-0.010, y=0.025+0.030, label= round(quantile(dflow,0.025),3)) +
          
          #Point RL
          geom_segment(aes(x = quantile(dfhigh,0.025), y = 0.025-0.02, xend = quantile(dfhigh,0.025), yend = 0.025+0.02)) +
          geom_segment(aes(x = (quantile(dfhigh,0.025)-0.005), y = 0.025, xend = (quantile(dfhigh,0.025)+0.005), yend = 0.025)) +
          annotate("text", x=quantile(dfhigh,0.025)+0.005, y=0.025+0.030, label= round(quantile(dfhigh,0.025),3)) +
          
          # Point LH
          geom_segment(aes(x = quantile(dflow,0.975), y = 0.975-0.02, xend = quantile(dflow,0.975), yend = 0.975+0.02)) +
          geom_segment(aes(x = (quantile(dflow,0.975)-0.005), y = 0.975, xend = (quantile(dflow,0.975)+0.005), yend = 0.975)) +
          annotate("text", x=quantile(dflow,0.975)-0.010, y=0.975-0.030, label= round(quantile(dflow,0.975),3)) +
          
          # Point RL
          geom_segment(aes(x = quantile(dfhigh,0.975), y = 0.975-0.02, xend = quantile(dfhigh,0.975), yend = 0.975+0.02)) +
          geom_segment(aes(x = (quantile(dfhigh,0.975)-0.005), y = 0.975, xend = (quantile(dfhigh,0.975)+0.005), yend = 0.975)) +
          annotate("text", x=quantile(dfhigh,0.975)-0.010, y=0.975-0.030, label= round(quantile(dfhigh,0.975),3)) +
          
          ggtitle("ECDF for IRR from Simulation (Cumulative Density Function)") +
          xlab("IRR") +
          ylab("Probability")
        
        fig
        
      }
    })
    
    # create uncertainty statements
    
    output$uncertainty_statements_s12 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[4])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[2])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #f0f8ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #4CAF50;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[9])
        )
        
        # Return all three div elements
        tagList(div1, div2, div3)
      }
    })  
    
    # add summary comment
    
    output$summary_comment_s12 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[12])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Recommendations
    
    output$recommendation_statement_s12 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          p(recommendation_list[1])
        )
        
        tagList(div1)
      }
    })  
    
    ## End System 12                                ##
    
    ## System 13                                    ##
    ## PNC = Epistemic              ##
    ## PC = Point                   ##
    ## PTARG = Aleatoric            ##
    
    ## Create Subtitle
    
    output$subtitle_s13 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is an ",type_list[2], ", PC is a ", type_list[1], ", Target is a ",type_list[3]))
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    }) 
    
    ## Create data
    
    analysis_results_13 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        PNC_lower <- isolate(input$PF_NC_lower)
        PNC_upper <- isolate(input$PF_NC_upper)
        PNC <- c(PNC_lower, PNC_upper)
        PC_lower <- isolate(input$PF_C_lower)
        PC_upper <- isolate(input$PF_C_upper)
        PC <- c(PC_lower,PC_upper)
        PoF <- isolate(input$PF_TARG)
        
        # Validate Constraints
        
        validate(
          
          #Epistemic Ranges
          need(PC_lower < PC_upper,"- The Probability of Failure given Conformance lower is greater than the upper boundary for the Probability of Failure given Conformance)" ),
          need(PNC_lower < PNC_upper,"- The Probability of Failure given Conformance lower is greater than the upper boundary for the Probability of Failure given Conformance)" ),
          
          #comparisons to Target
          need(PNC_lower >= PoF,"- The Probability of Failure given Nonconformance is less than the Target Probability of Failure which produces invalid probability for the IRR (the numerator becomes a negative number making the ratio less than 0)"),
          need(PC_upper <= PoF,"- The Probability of Failure given Conformance is greater than the Target Probability of Failure which produces invalid probability for the IRR (the denominator is less than the numerator, making the ratio greater than 1)"),
          
          #comparisons between A & B
          need(PC_lower != PNC_upper,"- The Probability of Failure given Conformance lower boundary is equal to Probability of Failure given Nononformance which produces invalid probability for the IRR (the denominator goes to zero violating a fundamental mathematical principle)"),
          need(PC_lower != PNC_lower,"- The Probability of Failure given Conformance lower boundary is equal to Probability of Failure given Nononformance which produces invalid probability for the IRR (the denominator goes to zero violating a fundamental mathematical principle)"),
          need(PC_upper != PNC_upper,"- The Probability of Failure given Conformance lower boundary is equal to Probability of Failure given Nononformance which produces invalid probability for the IRR (the denominator goes to zero violating a fundamental mathematical principle)"),
          need(PC_upper != PNC_lower,"- The Probability of Failure given Conformance upper boundary is equal to Probability of Failure given Nononformance which produces invalid probability for the IRR (the denominator goes to zero violating a fundamental mathematical principle)")
        )
        
        # Create dataframe
        
        df <- expand.grid(PNC,PC,PoF)
        names(df) <- c("PNC", "PC", "Target")
        
        # Add IRR
        df$IRR <- (df$PNC-df$Target)/(df$PNC-df$PC)
        
        return(df)
        
      }
    })
    
    ## Results
    
    # build results table
    
    output$epi_PoF_table_s13 <- renderTable({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        analysis_results_13()
        
      }
    },
    digits = 9,
    striped = TRUE,
    caption = "Table of IRR Estimates",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # add results comment
    
    output$results_comment_s13 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[13])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Summary 
    
    # build summary table
    
    output$epi_values_table_s13 <- renderTable({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        min_IRR <- min(analysis_results_13()$IRR)
        max_IRR <- max(analysis_results_13()$IRR)
        
        data.frame(
          Parameter = c("Minimum IRR", "Maximum IRR", "Epistemic Value"),
          Value = c(
            sprintf("%.6f", min_IRR),
            sprintf("%.6f", max_IRR),
            sprintf("%.6f", max_IRR)
          ),
          stringsAsFactors = FALSE
        )
        
      }
    }, 
    striped = TRUE, 
    hover = TRUE, 
    bordered = TRUE,
    caption = "Summary Table of minimum and maximum IRR Estimates",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # create uncertainty statements
    
    output$uncertainty_statements_s13 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[4])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[5])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[3])
        )
        
        # Return all div elements
        
        tagList(div1, div2, div3)
      }
    })  
    
    # add summary comment
    
    output$summary_comment_s13 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[13])
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    })
    
    ## Create Recommendations
    
    output$recommendation_statement_s13 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          p(recommendation_list[1])
        )
        
        tagList(div1)
      }
    })  
    
    ## End System 13                                   ##
    
    ## System 14                                       ##
    ## PNC = Epistemic              ##
    ## PC = Point                   ##
    ## PTARG = Aleatoric            ##
    
    ## Create Subtitle
    
    output$subtitle_s14 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is an ",type_list[2], ", PC is a ", type_list[2], ", Target is a ",type_list[2]))
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    }) 
    
    ## Create data
    
    analysis_results_14 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        # Create Vectors
        
        PNC_lower <- isolate(input$PF_NC_lower)
        PNC_upper <- isolate(input$PF_NC_upper)
        PNC <- c(PNC_lower, PNC_upper)
        PC_lower <- isolate(input$PF_C_lower)
        PC_upper <- isolate(input$PF_C_upper)
        PC <- c(PC_lower,PC_upper)
        PoF_lower <- isolate(input$PF_TARG_lower)
        PoF_upper <- isolate(input$PF_TARG_upper)
        PoF <- c(PoF_lower,PoF_upper)
        
        # Validate Constraints
        
        validate(
          
          #Epistemic Range Orientation
          need(PC_lower < PC_upper,"- The Probability of Failure given Conformance lower is greater than the upper boundary for the Probability of Failure given Conformance)" ),
          need(PNC_lower < PNC_upper,"- The Probability of Failure given Conformance lower is greater than the upper boundary for the Probability of Failure given Conformance)" ),
          need(PoF_lower < PoF_upper,"- The Probability of Failure given Conformance lower is greater than the upper boundary for the Probability of Failure given Conformance)" ),
          
          #comparisons to Target
          need(PNC_lower >= PoF_upper,"- The Probability of Failure given Nonconformance is less than the Target Probability of Failure which produces invalid probability for the IRR (the numerator becomes a negative number making the ratio less than 0)"),
          need(PC_upper <= PoF_lower,"- The Probability of Failure given Conformance is greater than the Target Probability of Failure which produces invalid probability for the IRR (the denominator is less than the numerator, making the ratio greater than 1)"),
          
          #comparisons between A & B
          need(PC_lower != PNC_upper,"- The Probability of Failure given Conformance lower boundary is equal to Probability of Failure given Nononformance which produces invalid probability for the IRR (the denominator goes to zero violating a fundamental mathematical principle)"),
          need(PC_lower != PNC_lower,"- The Probability of Failure given Conformance lower boundary is equal to Probability of Failure given Nononformance which produces invalid probability for the IRR (the denominator goes to zero violating a fundamental mathematical principle)"),
          need(PC_upper != PNC_upper,"- The Probability of Failure given Conformance lower boundary is equal to Probability of Failure given Nononformance which produces invalid probability for the IRR (the denominator goes to zero violating a fundamental mathematical principle)"),
          need(PC_upper != PNC_lower,"- The Probability of Failure given Conformance upper boundary is equal to Probability of Failure given Nononformance which produces invalid probability for the IRR (the denominator goes to zero violating a fundamental mathematical principle)")
        )
        
        # Create dataframe
        
        df <- expand.grid(PNC,PC,PoF)
        names(df) <- c("PNC", "PC", "Target")
        
        # Add IRR
        df$IRR <- (df$PNC-df$Target)/(df$PNC-df$PC)
        
        return(df)
        
      }
    })
    
    ## Results
    
    # build results table
    
    output$epi_PoF_table_s14 <- renderTable({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        analysis_results_14()
        
      }
    },
    digits = 9,
    striped = TRUE,
    caption = "Table of IRR Estimates",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # add results comment
    
    output$results_comment_s14 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[14])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Summary 
    
    # build summary table
    
    output$epi_values_table_s14 <- renderTable({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        min_IRR <- min(analysis_results_14()$IRR)
        max_IRR <- max(analysis_results_14()$IRR)
        
        data.frame(
          Parameter = c("Minimum IRR", "Maximum IRR", "Epistemic Value"),
          Value = c(
            sprintf("%.6f", min_IRR),
            sprintf("%.6f", max_IRR),
            sprintf("%.6f", max_IRR)
          ),
          stringsAsFactors = FALSE
        )
        
      }
    }, 
    striped = TRUE, 
    hover = TRUE, 
    bordered = TRUE,
    caption = "Summary Table of minimum and maximum IRR Estimates",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # create uncertainty statements
    
    output$uncertainty_statements_s14 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[4])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[5])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[3])
        )
        
        # Return all div elements
        
        tagList(div1, div2, div3)
      }
    })  
    
    # add summary comment
    
    output$summary_comment_s14 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[14])
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    })
    
    ## Create Recommendations
    
    output$recommendation_statement_s14 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          p(recommendation_list[2])
        )
        
        tagList(div1)
      }
    })  
    
    ## End System 14                                   ##
    
    ## System 15  ##
    ## PNC = Epistemic              ##
    ## PC = Point                   ##
    ## PTARG = Aleatoric            ##
    
    ## Create Subtitle
    
    output$subtitle_s15 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is an ",type_list[2], ", PC is a ", type_list[2], ", Target is a ",type_list[3]))
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    }) 
    
    ## Create data
    
    analysis_results_15 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        # Create Vectors
        
        PNC_lower <- isolate(input$PF_NC_lower)
        PNC_upper <- isolate(input$PF_NC_upper)
        PNC <- c(PNC_lower, PNC_upper)
        PC_lower <- isolate(input$PF_C_lower)
        PC_upper <- isolate(input$PF_C_upper)
        PC <- c(PC_lower, PC_upper)
        
        if(input$TARG_in == "TARG_norm"){
          # Normal distribution
          PTARG_mean <- isolate(input$PF_TARG_mean)
          PTARG_sd <- isolate(input$PF_TARG_sd)
          PoF <- rnorm(100000, mean = PTARG_mean, sd = PTARG_sd)
        } 
        else if(input$TARG_in == "TARG_beta"){
          PTARG_shape1 <- isolate(input$PF_TARG_shape1)
          PTARG_shape2 <- isolate(input$PF_TARG_shape2)
          PoF <- rbeta(100000, shape1 = PTARG_shape1, shape2 = PTARG_shape2)
        }
        else if(input$TARG_in == "TARG_lgnorm"){
          PTARG_lgMmean <- isolate(input$PF_TARG_lgMean)
          PTARG_lgSD <- isolate(input$PF_TARG_lgSD)
          PoF <- rlnorm(100000, meanlog = PTARG_lgMean, sdlog = PTARG_lgSD)
        }
        
        test <- expand.grid(PNC,PC)
        df <- data.frame("PNC" = rep(test$Var1,25000),"PC" = rep(test$Var2,25000))
        
        df$PoF <- PoF
        
        df$IRR <- round((df$PNC - df$PoF)/(df$PNC - df$PC),digits = 9)
        
        # Check Constraints
        
        df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
        
        df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
        
        return(df)
        
      }
    })
    
    ## Results
    
    # create results histogram
    
    output$aleatoric_hist_s15 <- renderPlotly({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        fig <- plot_ly(type = "histogram", alpha = 0.8) 
        
        fig <- fig %>% add_histogram(x = ~analysis_results_15()[analysis_results_15()$PNC == input$PF_NC_lower & analysis_results_15()$PC == input$PF_C_lower,"IRR"], name = "PNC lower & PC lower")
        
        fig <- fig %>% add_histogram(x = ~analysis_results_15()[analysis_results_15()$PNC == input$PF_NC_upper & analysis_results_15()$PC == input$PF_C_lower,"IRR"], name = "PNC upper & PC lower")
        
        fig <- fig %>% add_histogram(x = ~analysis_results_15()[analysis_results_15()$PNC == input$PF_NC_lower & analysis_results_15()$PC == input$PF_C_upper,"IRR"], name = "PNC lower & PC upper")
        
        fig <- fig %>% add_histogram(x = ~analysis_results_15()[analysis_results_15()$PNC == input$PF_NC_upper & analysis_results_15()$PC == input$PF_C_upper,"IRR"], name = "PNC upper & PC upper")
        
        fig <- fig %>%
          layout(
            xaxis = list(title = "IRR Distribution"),
            yaxis = list(title = "Frequency",
                         barmode = "overlay")
          )
        
        fig <- fig %>% config(
          displayModeBar = TRUE,  # Show the toolbar
          modeBarButtonsToRemove = c(
            'select2d',
            'lasso2d',
            'autoScale2d',
            'hoverClosestCartesian',
            'hoverCompareCartesian',
            'toggleSpikelines'
          ),
          displaylogo = FALSE,  # Remove plotly logo
          toImageButtonOptions = list(
            format = 'png',
            filename = 'IRR_plot',
            height = 600,
            width = 800,
            scale = 1
          )
        )
        
        fig
        
      }
    })
    
    #create measure of utilization
    
    output$count_data_s15 <- renderPrint({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        value <- nrow(analysis_results_15())/100000
        paste("Fraction of simulated values included in the histogram", value)
        
      }
    })
    
    # add results comment
    
    output$results_comment_s15 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[15])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Summary
    
    # create summarization table
    
    output$quantile_data_s15 <- renderTable({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        quantiles <- round(quantile(analysis_results_15()$IRR, probs = c(0.95, 0.975, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999, 0.99999999, 0.999999999)),digits = 9)
        quantile_table <- (data.frame(
          Quantile = names(quantiles),
          Value = as.numeric(quantiles)
        ))
        
      }
    }, 
    digits = 9,
    caption = "Maximum IRR Values",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # create 95% interval
    
    output$interval_data_s15 <- renderTable({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        PNClowPClow <- analysis_results_15()[analysis_results_15()$PNC == min(analysis_results_15()$PNC) & analysis_results_15()$PC == min(analysis_results_15()$PC),"IRR"]
        PNChighPClow <- analysis_results_15()[analysis_results_15()$PNC == max(analysis_results_15()$PNC) & analysis_results_15()$PC == min(analysis_results_15()$PC),"IRR"]
        PNClowPChigh <- analysis_results_15()[analysis_results_15()$PNC == min(analysis_results_15()$PNC) & analysis_results_15()$PC == max(analysis_results_15()$PC),"IRR"]
        PNChighPChigh <- analysis_results_15()[analysis_results_15()$PNC == max(analysis_results_15()$PNC) & analysis_results_15()$PC == max(analysis_results_15()$PC),"IRR"]
        
        quantile_table <- data.frame("PNC" = c(min(analysis_results_15()$PNC),min(analysis_results_15()$PNC),max(analysis_results_15()$PNC),max(analysis_results_15()$PNC)),
                                     "PC" = c(min(analysis_results_15()$PC),max(analysis_results_15()$PC),min(analysis_results_15()$PC),max(analysis_results_15()$PC)),
                                     "0.025" = c(quantile(PNClowPClow,0.025),quantile(PNChighPClow,0.025),quantile(PNClowPChigh,0.025),quantile(PNChighPChigh,0.025)),
                                     "0.975" = c(quantile(PNClowPClow,0.975),quantile(PNChighPClow,0.975),quantile(PNClowPChigh,0.975),quantile(PNChighPChigh,0.975))
        )
        return(quantile_table)
        
      }
    }, 
    digits = 9, 
    caption = "95% Interval",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    # create ecdf plot
    
    output$ecdf_plot_s15 <- renderPlot({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        PNClowPClow <- analysis_results_15()[analysis_results_15()$PNC == min(analysis_results_15()$PNC) & analysis_results_15()$PC == min(analysis_results_15()$PC),"IRR"]
        PNChighPClow <- analysis_results_15()[analysis_results_15()$PNC == max(analysis_results_15()$PNC) & analysis_results_15()$PC == min(analysis_results_15()$PC),"IRR"]
        PNClowPChigh <- analysis_results_15()[analysis_results_15()$PNC == min(analysis_results_15()$PNC) & analysis_results_15()$PC == max(analysis_results_15()$PC),"IRR"]
        PNChighPChigh <- analysis_results_15()[analysis_results_15()$PNC == max(analysis_results_15()$PNC) & analysis_results_15()$PC == max(analysis_results_15()$PC),"IRR"]
        
        fig <-ggplot() +
          stat_ecdf(aes(PNClowPClow), color = "orange") +
          stat_ecdf(aes(PNChighPClow), color = "red") +
          stat_ecdf(aes(PNClowPChigh), color = "green") +
          stat_ecdf(aes(PNChighPChigh), color = "purple") +
          
          #Point LL
          geom_segment(aes(x = quantile(PNClowPClow,0.025), y = 0.025-0.02, xend = quantile(PNClowPClow,0.025), yend = 0.025+0.02)) +
          geom_segment(aes(x = (quantile(PNClowPClow,0.025)-0.005), y = 0.025, xend = (quantile(PNClowPClow,0.025)+0.005), yend = 0.025)) +
          annotate("text", x=quantile(PNClowPClow,0.025)-0.010, y=0.025+0.030, label= round(quantile(PNClowPClow,0.025),3)) +
          
          #Point RL
          geom_segment(aes(x = quantile(PNChighPChigh,0.025), y = 0.025-0.02, xend = quantile(PNChighPChigh,0.025), yend = 0.025+0.02)) +
          geom_segment(aes(x = (quantile(PNChighPChigh,0.025)-0.005), y = 0.025, xend = (quantile(PNChighPChigh,0.025)+0.005), yend = 0.025)) +
          annotate("text", x=quantile(PNChighPChigh,0.025)+0.005, y=0.025+0.030, label= round(quantile(PNChighPChigh,0.025),3)) +
          
          # Point LH
          geom_segment(aes(x = quantile(PNClowPClow,0.975), y = 0.975-0.02, xend = quantile(PNClowPClow,0.975), yend = 0.975+0.02)) +
          geom_segment(aes(x = (quantile(PNClowPClow,0.975)-0.005), y = 0.975, xend = (quantile(PNClowPClow,0.975)+0.005), yend = 0.975)) +
          annotate("text", x=quantile(PNClowPClow,0.975)-0.010, y=0.975-0.030, label= round(quantile(PNClowPClow,0.975),3)) +
          
          # Point RL
          geom_segment(aes(x = quantile(PNChighPChigh,0.975), y = 0.975-0.02, xend = quantile(PNChighPChigh,0.975), yend = 0.975+0.02)) +
          geom_segment(aes(x = (quantile(PNChighPChigh,0.975)-0.005), y = 0.975, xend = (quantile(PNChighPChigh,0.975)+0.005), yend = 0.975)) +
          annotate("text", x=quantile(PNChighPChigh,0.975)-0.010, y=0.975-0.030, label= round(quantile(PNChighPChigh,0.975),3)) +
          
          ggtitle("ECDF for IRR from Simulation (Cumulative Density Function)") +
          xlab("IRR") +
          ylab("Probability")
        
        fig
        
      }
    })
    
    # create uncertainty statements
    
    output$uncertainty_statements_s15 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[4])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[5])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #f0f8ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #4CAF50;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[9])
        )
        
        # Return all three div elements
        tagList(div1, div2, div3)
      }
    })  
    
    # add summary comment
    
    output$summary_comment_s15 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[15])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Recommendations
    
    output$recommendation_statement_s15 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          p(recommendation_list[2])
        )
        
        tagList(div1)
      }
    })  
    
    ## End System 15                 ##
    
    ## System 16                     ##
    ## PNC = Epistemic              ##
    ## PC = Point                   ##
    ## PTARG = Aleatoric            ##
    
    ## Create Subtitle
    
    output$subtitle_s16 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is an ",type_list[2], ", PC is an ", type_list[3], ", Target is a ",type_list[1]))
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    }) 
    
    ## Create data
    
    analysis_results_16 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        # Create Vectors
        
        PNC_lower <- isolate(input$PF_NC_lower)
        PNC_upper <- isolate(input$PF_NC_upper)
        PNC <- c(PNC_lower, PNC_upper)
        
        if(input$PC_in == "PC_norm"){
          # Normal distribution
          PC_mean <- isolate(input$PF_C_mean)
          PC_sd <- isolate(input$PF_C_sd)
          PC <- rnorm(100000, mean = PC_mean, sd = PC_sd)
        } 
        else if(input$PC_in == "PC_beta"){
          # Beta distribution
          PC_shape1 <- isolate(input$PF_C_shape1)
          PC_shape2 <- isolate(input$PF_C_shape2)
          PC <- rbeta(100000, shape1 = PC_shape1, shape2 = PC_shape2)
        }
        else if(input$PC_in == "PC_lgnorm"){
          # Log Normal distribution
          PC_lgMean <- isolate(input$PF_C_lgMean)
          PC_lgSD <- isolate(input$PF_C_lgSD)
          PC <- rlnorm(100000, meanlog = PC_lgMean, sdlog = PC_lgSD)
        }
        
        PoF <- isolate(input$PF_TARG)
        
        # Validate Constraints
        
        # Create dataframe
        
        df <- data.frame("PNC" = PNC, "PC" = PC, "PoF" = PoF)
        
        # Add IRR
        
        df$IRR <- round((df$PNC - df$PoF)/(df$PNC - df$PC),digits = 9)
        
        # Check Constraints
        
        df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
        
        df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
        
        return(df)
        
      }
    })
    
    # create results histogram
    
    output$aleatoric_hist_s16 <- renderPlotly({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        fig <- plot_ly(type = "histogram", alpha = 0.8) 
        
        fig <- fig %>% add_histogram(x = ~analysis_results_16()[analysis_results_16()$PNC == input$PF_NC_lower,"IRR"], name = "PNC lower")
        
        fig <- fig %>% add_histogram(x = ~analysis_results_16()[analysis_results_16()$PNC == input$PF_NC_upper,"IRR"], name = "PNC upper")
        
        fig <- fig %>%
          layout(
            xaxis = list(title = "IRR Distribution"),
            yaxis = list(title = "Frequency",
                         barmode = "overlay")
          )
        
        fig <- fig %>% config(
          displayModeBar = TRUE,  # Show the toolbar
          modeBarButtonsToRemove = c(
            'select2d',
            'lasso2d',
            'autoScale2d',
            'hoverClosestCartesian',
            'hoverCompareCartesian',
            'toggleSpikelines'
          ),
          displaylogo = FALSE,  # Remove plotly logo
          toImageButtonOptions = list(
            format = 'png',
            filename = 'IRR_plot',
            height = 600,
            width = 800,
            scale = 1
          )
        )
        
        fig
        
      }
    })
    
    # create measure of utilization
    
    output$count_data_s16 <- renderPrint({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        value <- nrow(analysis_results_16())/100000
        paste("Fraction of simulated values included in the histogram", value)
        
      }
    })
    
    
    # add results comment
    
    output$results_comment_s16 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[16])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Summary
    
    # create summarization table
    
    output$quantile_data_s16 <- renderTable({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        quantiles <- round(quantile(analysis_results_16()$IRR, probs = c(0.95, 0.975, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999, 0.99999999, 0.999999999)),digits = 9)
        quantile_table <- (data.frame(
          Quantile = names(quantiles),
          Value = as.numeric(quantiles)
        ))
        
      }
    }, 
    digits = 9,
    caption = "Maximum IRR Values",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # create 95% interval
    
    output$interval_data_s16 <- renderTable({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        dflow <- analysis_results_16()[analysis_results_16()$PNC == min(analysis_results_16()$PNC),"IRR"]
        dfhigh <- analysis_results_16()[analysis_results_16()$PNC == max(analysis_results_16()$PNC),"IRR"]
        
        quantile_table <- data.frame("PNC" = c(min(analysis_results_16()$PNC),max(analysis_results_16()$PNC)),
                                     "0.025" = c(quantile(dflow,0.025),quantile(dfhigh,0.025)),
                                     "0.975" = c(quantile(dflow,0.975),quantile(dfhigh,0.975))
        )
        return(quantile_table)
        
      }
    }, 
    digits = 9, 
    caption = "95% Interval",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    # create ecdf plot
    
    output$ecdf_plot_s16 <- renderPlot({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        dflow <- analysis_results_16()[analysis_results_16()$PNC == min(analysis_results_16()$PNC),"IRR"]
        dfhigh <- analysis_results_16()[analysis_results_16()$PNC == max(analysis_results_16()$PNC),"IRR"]
        
        fig <-ggplot() +
          stat_ecdf(aes(dflow), color = "darkblue") +
          stat_ecdf(aes(dfhigh), color = "red") +
          
          #Point LL
          geom_segment(aes(x = quantile(dflow,0.025), y = 0.025-0.02, xend = quantile(dflow,0.025), yend = 0.025+0.02)) +
          geom_segment(aes(x = (quantile(dflow,0.025)-0.005), y = 0.025, xend = (quantile(dflow,0.025)+0.005), yend = 0.025)) +
          annotate("text", x=quantile(dflow,0.025)-0.010, y=0.025+0.030, label= round(quantile(dflow,0.025),3)) +
          
          #Point RL
          geom_segment(aes(x = quantile(dfhigh,0.025), y = 0.025-0.02, xend = quantile(dfhigh,0.025), yend = 0.025+0.02)) +
          geom_segment(aes(x = (quantile(dfhigh,0.025)-0.005), y = 0.025, xend = (quantile(dfhigh,0.025)+0.005), yend = 0.025)) +
          annotate("text", x=quantile(dfhigh,0.025)+0.005, y=0.025+0.030, label= round(quantile(dfhigh,0.025),3)) +
          
          # Point LH
          geom_segment(aes(x = quantile(dflow,0.975), y = 0.975-0.02, xend = quantile(dflow,0.975), yend = 0.975+0.02)) +
          geom_segment(aes(x = (quantile(dflow,0.975)-0.005), y = 0.975, xend = (quantile(dflow,0.975)+0.005), yend = 0.975)) +
          annotate("text", x=quantile(dflow,0.975)-0.010, y=0.975-0.030, label= round(quantile(dflow,0.975),3)) +
          
          # Point RL
          geom_segment(aes(x = quantile(dfhigh,0.975), y = 0.975-0.02, xend = quantile(dfhigh,0.975), yend = 0.975+0.02)) +
          geom_segment(aes(x = (quantile(dfhigh,0.975)-0.005), y = 0.975, xend = (quantile(dfhigh,0.975)+0.005), yend = 0.975)) +
          annotate("text", x=quantile(dfhigh,0.975)-0.010, y=0.975-0.030, label= round(quantile(dfhigh,0.975),3)) +
          
          ggtitle("ECDF for IRR from Simulation (Cumulative Density Function)") +
          xlab("IRR") +
          ylab("Probability")
        
        fig
        
      }
    })
    
    # create uncertainty statements
    
    output$uncertainty_statements_s16 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[4])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #f0f8ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #4CAF50;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[8])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[3])
        )
        
        # Return all three div elements
        tagList(div1, div2, div3)
      }
    })  
    
    # add summary comment
    
    output$summary_comment_s16 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[16])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Recommendations
    
    output$recommendation_statement_s16 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          p(recommendation_list[1])
        )
        
        tagList(div1)
      }
    })  
    
    ## End System 16                 ##
    
    ## System 17                    ##
    ## PNC = Epistemic              ##
    ## PC = Point                   ##
    ## PTARG = Aleatoric            ##
    
    ## Create Subtitle
    
    output$subtitle_s17 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is an ",type_list[2], ", PC is an ", type_list[3], ", Target is a ",type_list[2]))
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    }) 
    
    ## Create data
    
    analysis_results_17 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        # Create Vectors
        
        PNC_lower <- isolate(input$PF_NC_lower)
        PNC_upper <- isolate(input$PF_NC_upper)
        PNC <- c(PNC_lower, PNC_upper)
        
        if(input$PC_in == "PC_norm"){
          # Normal distribution
          PC_mean <- isolate(input$PF_C_mean)
          PC_sd <- isolate(input$PF_C_sd)
          PC <- rnorm(100000, mean = PC_mean, sd = PC_sd)
        } 
        else if(input$PC_in == "PC_beta"){
          # Beta distribution
          PC_shape1 <- isolate(input$PF_C_shape1)
          PC_shape2 <- isolate(input$PF_C_shape2)
          PC <- rbeta(100000, shape1 = PC_shape1, shape2 = PC_shape2)
        }
        else if(input$PC_in == "PC_lgnorm"){
          # Log Normal distribution
          PC_lgMean <- isolate(input$PF_C_lgMean)
          PC_lgSD <- isolate(input$PF_C_lgSD)
          PC <- rlnorm(100000, meanlog = PC_lgMean, sdlog = PC_lgSD)
        }
        
        PoF_lower <- isolate(input$PF_TARG_lower)
        PoF_upper <- isolate(input$PF_TARG_upper)
        PoF <- c(PoF_lower, PoF_upper)
        
        # Validate constraints
        
        # Create dataframe
        test <- expand.grid(PNC,PoF)
        names(test) <- c("PNC","PoF")
        df <- data.frame("PNC" = rep(test$PNC,25000),"PC" = PC,"PoF" = rep(test$PoF,25000))
        
        # Add IRR
        
        df$IRR <- round((df$PNC - df$PoF)/(df$PNC - df$PC),digits = 9)
        
        # Check Constraints
        
        df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
        
        df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
        
        return(df)
        
      }
    })
    
    # create results histogram
    
    output$aleatoric_hist_s17 <- renderPlotly({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        fig <- plot_ly(type = "histogram", alpha = 0.8) 
        
        fig <- fig %>% add_histogram(x = ~analysis_results_17()[analysis_results_17()$PNC == input$PF_NC_lower & analysis_results_17()$PoF == input$PF_TARG_lower,"IRR"], name = "PNC lower & Target lower")
        
        fig <- fig %>% add_histogram(x = ~analysis_results_17()[analysis_results_17()$PNC == input$PF_NC_upper & analysis_results_17()$PoF == input$PF_TARG_lower,"IRR"], name = "PNC upper & Target lower")
        
        fig <- fig %>% add_histogram(x = ~analysis_results_17()[analysis_results_17()$PNC == input$PF_NC_lower & analysis_results_17()$PoF == input$PF_TARG_upper,"IRR"], name = "PNC lower & Target upper")
        
        fig <- fig %>% add_histogram(x = ~analysis_results_17()[analysis_results_17()$PNC == input$PF_NC_upper & analysis_results_17()$PoF == input$PF_TARG_upper,"IRR"], name = "PNC upper & Target upper")
        
        fig <- fig %>%
          layout(
            xaxis = list(title = "IRR Distribution"),
            yaxis = list(title = "Frequency",
                         barmode = "overlay")
          )
        
        fig <- fig %>% config(
          displayModeBar = TRUE,  # Show the toolbar
          modeBarButtonsToRemove = c(
            'select2d',
            'lasso2d',
            'autoScale2d',
            'hoverClosestCartesian',
            'hoverCompareCartesian',
            'toggleSpikelines'
          ),
          displaylogo = FALSE,  # Remove plotly logo
          toImageButtonOptions = list(
            format = 'png',
            filename = 'IRR_plot',
            height = 600,
            width = 800,
            scale = 1
          )
        )
        
        fig
        
      }
    })
    
    #create measure of utilization
    
    output$count_data_s17 <- renderPrint({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        value <- nrow(analysis_results_17())/100000
        paste("Fraction of simulated values included in the histogram", value)
        
      }
    })
    
    # add results comment
    
    output$results_comment_s17 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[17])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Summary
    
    # create summarization table
    
    output$quantile_data_s17 <- renderTable({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        quantiles <- round(quantile(analysis_results_17()$IRR, probs = c(0.95, 0.975, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999, 0.99999999, 0.999999999)),digits = 9)
        quantile_table <- (data.frame(
          Quantile = names(quantiles),
          Value = as.numeric(quantiles)
        ))
        
      }
    }, 
    digits = 9,
    caption = "Maximum IRR Values",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # create 95% interval
    
    output$interval_data_s17 <- renderTable({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        PNClowPoFlow <- analysis_results_17()[analysis_results_17()$PNC == min(analysis_results_17()$PNC) & analysis_results_17()$PoF == min(analysis_results_17()$PoF),"IRR"]
        PNChighPoFlow <- analysis_results_17()[analysis_results_17()$PNC == max(analysis_results_17()$PNC) & analysis_results_17()$PoF == min(analysis_results_17()$PoF),"IRR"]
        PNClowPoFhigh <- analysis_results_17()[analysis_results_17()$PNC == min(analysis_results_17()$PNC) & analysis_results_17()$PoF == max(analysis_results_17()$PoF),"IRR"]
        PNChighPoFhigh <- analysis_results_17()[analysis_results_17()$PNC == max(analysis_results_17()$PNC) & analysis_results_17()$PoF == max(analysis_results_17()$PoF),"IRR"]
        
        
        quantile_table <- data.frame("PNC" = c(min(analysis_results_17()$PNC),min(analysis_results_17()$PNC),max(analysis_results_17()$PNC),max(analysis_results_17()$PNC)),
                                     "PoF" = c(min(analysis_results_17()$PoF),max(analysis_results_17()$PoF),min(analysis_results_17()$PoF),max(analysis_results_17()$PoF)),
                                     "0.025" = c(quantile(PNClowPoFlow,0.025),quantile(PNChighPoFlow,0.025),quantile(PNClowPoFhigh,0.025),quantile(PNChighPoFhigh,0.025)),
                                     "0.975" = c(quantile(PNClowPoFlow,0.975),quantile(PNChighPoFlow,0.975),quantile(PNClowPoFhigh,0.975),quantile(PNChighPoFhigh,0.975))
        )
        return(quantile_table)
        
      }
    }, 
    digits = 9, 
    caption = "95% Interval",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    # create ecdf plot
    
    output$ecdf_plot_s17 <- renderPlot({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        PNClowPoFlow <- analysis_results_17()[analysis_results_17()$PNC == min(analysis_results_17()$PNC) & analysis_results_17()$PoF == min(analysis_results_17()$PoF),"IRR"]
        PNChighPoFlow <- analysis_results_17()[analysis_results_17()$PNC == max(analysis_results_17()$PNC) & analysis_results_17()$PoF == min(analysis_results_17()$PoF),"IRR"]
        PNClowPoFhigh <- analysis_results_17()[analysis_results_17()$PNC == min(analysis_results_17()$PNC) & analysis_results_17()$PoF == max(analysis_results_17()$PoF),"IRR"]
        PNChighPoFhigh <- analysis_results_17()[analysis_results_17()$PNC == max(analysis_results_17()$PNC) & analysis_results_17()$PoF == max(analysis_results_17()$PoF),"IRR"]
        
        # Combine into a data frame
        df <- data.frame(PNClowPoFlow = PNClowPoFlow, PNChighPoFlow = PNChighPoFlow, PNClowPoFhigh = PNClowPoFhigh, PNChighPoFhigh = PNChighPoFhigh)
        
        # Compute medians
        #medians <- sapply(df, median)
        lower_quantiles <- sapply(df, function(x) mean(quantile(x, probs = c(0.05, 0.10, 0.15))))
        
        
        # Get names of lowest and highest median vectors
        lowest_name <- names(which.min(lower_quantiles))
        highest_name <- names(which.max(lower_quantiles))
        
        # Convert to symbols for ggplot
        lowest_data <- df[[lowest_name]]
        highest_data <- df[[highest_name]]
        
        fig <-ggplot() +
          stat_ecdf(aes(PNClowPoFlow), color = "orange") +
          stat_ecdf(aes(PNChighPoFlow), color = "red") +
          stat_ecdf(aes(PNClowPoFhigh), color = "green") +
          stat_ecdf(aes(PNChighPoFhigh), color = "purple") +
          
          #Point LL
          geom_segment(aes(x = quantile(lowest_data,0.025), y = 0.025-0.02, xend = quantile(lowest_data,0.025), yend = 0.025+0.02)) +
          geom_segment(aes(x = (quantile(lowest_data,0.025)-0.005), y = 0.025, xend = (quantile(lowest_data,0.025)+0.005), yend = 0.025)) +
          annotate("text", x=quantile(lowest_data,0.025)-0.010, y=0.025+0.030, label= round(quantile(lowest_data,0.025),3)) +
          
          #Point RL
          geom_segment(aes(x = quantile(highest_data,0.025), y = 0.025-0.02, xend = quantile(highest_data,0.025), yend = 0.025+0.02)) +
          geom_segment(aes(x = (quantile(highest_data,0.025)-0.005), y = 0.025, xend = (quantile(highest_data,0.025)+0.005), yend = 0.025)) +
          annotate("text", x=quantile(highest_data,0.025)+0.005, y=0.025+0.030, label= round(quantile(highest_data,0.025),3)) +
          
          # Point LH
          geom_segment(aes(x = quantile(lowest_data,0.975), y = 0.975-0.02, xend = quantile(lowest_data,0.975), yend = 0.975+0.02)) +
          geom_segment(aes(x = (quantile(lowest_data,0.975)-0.005), y = 0.975, xend = (quantile(lowest_data,0.975)+0.005), yend = 0.975)) +
          annotate("text", x=quantile(lowest_data,0.975)-0.010, y=0.975-0.030, label= round(quantile(lowest_data,0.975),3)) +
          
          # Point RL
          geom_segment(aes(x = quantile(highest_data,0.975), y = 0.975-0.02, xend = quantile(highest_data,0.975), yend = 0.975+0.02)) +
          geom_segment(aes(x = (quantile(highest_data,0.975)-0.005), y = 0.975, xend = (quantile(highest_data,0.975)+0.005), yend = 0.975)) +
          annotate("text", x=quantile(highest_data,0.975)-0.010, y=0.975-0.030, label= round(quantile(highest_data,0.975),3)) +
          
          ggtitle("ECDF for IRR from Simulation (Cumulative Density Function)") +
          xlab("IRR") +
          ylab("Probability")
        
        fig
        
      }
    })
    
    # create uncertainty statements
    
    output$uncertainty_statements_s17 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[4])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #f0f8ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #4CAF50;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[5])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[9])
        )
        
        # Return all three div elements
        tagList(div1, div2, div3)
      }
    })  
    
    # add summary comment
    
    output$summary_comment_s17 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[17])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Recommendations
    
    output$recommendation_statement_s17 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          p(recommendation_list[2])
        )
        
        tagList(div1)
      }
    })  
    
    ## End System 17                 ##
    
    ## System 18  ##
    ## PNC = Epistemic              ##
    ## PC = Aleatoric               ##
    ## PTARG = Aleatoric            ##
    
    ## Create Subtitle
    
    output$subtitle_s18 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is an ",type_list[2], ", PC is an ", type_list[3], ", Target is a ",type_list[2]))
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    }) 
    
    ## Create data
    
    analysis_results_18 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        PNC_lower <- isolate(input$PF_NC_lower)
        PNC_upper <- isolate(input$PF_NC_upper)
        PNC <- c(PNC_lower,PNC_upper)
        
        if(input$PC_in == "PC_norm"){
          # Normal distribution
          PC_mean <- isolate(input$PF_C_mean)
          PC_sd <- isolate(input$PF_C_sd)
          PC <- rnorm(100000, mean = PC_mean, sd = PC_sd)
        } 
        else if(input$PC_in == "PC_beta"){
          # Beta distribution
          PC_shape1 <- isolate(input$PF_C_shape1)
          PC_shape2 <- isolate(input$PF_C_shape2)
          PC <- rbeta(100000, shape1 = PC_shape1, shape2 = PC_shape2)
        }
        else if(input$PC_in == "PC_lgnorm"){
          # Log Normal distribution
          PC_lgMean <- isolate(input$PF_C_lgMean)
          PC_lgSD <- isolate(input$PF_C_lgSD)
          PC <- rlnorm(100000, meanlog = PC_lgMean, sdlog = PC_lgSD)
        }
        
        if(input$TARG_in == "TARG_norm"){
          # Normal distribution
          PTARG_mean <- isolate(input$PF_TARG_mean)
          PTARG_sd <- isolate(input$PF_TARG_sd)
          PoF <- rnorm(100000, mean = PTARG_mean, sd = PTARG_sd)
        } 
        else if(input$TARG_in == "TARG_beta"){
          PTARG_shape1 <- isolate(input$PF_TARG_shape1)
          PTARG_shape2 <- isolate(input$PF_TARG_shape2)
          PoF <- rbeta(100000, shape1 = PTARG_shape1, shape2 = PTARG_shape2)
        }
        else if(input$TARG_in == "TARG_lgnorm"){
          PTARG_lgMmean <- isolate(input$PF_TARG_lgMean)
          PTARG_lgSD <- isolate(input$PF_TARG_lgSD)
          PoF <- rlnorm(100000, meanlog = PTARG_lgMean, sdlog = PTARG_lgSD)
        }
        
        # Validate Constraints
        
        # Create dataframe
        
        df <- data.frame("PNC" = rep(PNC, times = 50000), "PC" = PC, "PoF" = PoF)
        
        # Add IRR
        
        df$IRR <- round((df$PNC - df$PoF)/(df$PNC - df$PC),digits = 9)
        
        # Check Constraints
        
        df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
        
        df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
        
        return(df)
        
      }
    })
    
    # create results histogram
    
    output$aleatoric_hist_s18 <- renderPlotly({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        fig <- plot_ly(type = "histogram", alpha = 0.8) 
        
        fig <- fig %>% add_histogram(x = ~analysis_results_18()[analysis_results_18()$PNC == input$PF_NC_lower,"IRR"], name = "PNC lower")
        fig <- fig %>% add_histogram(x = ~analysis_results_18()[analysis_results_18()$PNC == input$PF_NC_upper,"IRR"], name = "PNC upper")
        
        fig <- fig %>%
          layout(
            xaxis = list(title = "IRR Distribution"),
            yaxis = list(title = "Frequency")
          )
        
        fig <- fig %>% config(
          displayModeBar = TRUE,  # Show the toolbar
          modeBarButtonsToRemove = c(
            'select2d',
            'lasso2d',
            'autoScale2d',
            'hoverClosestCartesian',
            'hoverCompareCartesian',
            'toggleSpikelines'
          ),
          displaylogo = FALSE,  # Remove plotly logo
          toImageButtonOptions = list(
            format = 'png',
            filename = 'IRR_plot',
            height = 600,
            width = 800,
            scale = 1
          )
        )
        
        fig
        
      }
    })
    
    # create measure of utilization
    
    output$count_data_s18 <- renderPrint({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        value <- nrow(analysis_results_18())/100000
        paste("Fraction of simulated values included in the histogram", value)
        
      }
    })
    
    # add results comment
    
    output$results_comment_s18 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[18])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Summary
    
    # create summarization table
    
    output$quantile_data_s18 <- renderTable({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        quantiles <- round(quantile(analysis_results_18()$IRR, probs = c(0.95, 0.975, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999, 0.99999999, 0.999999999)),digits = 9)
        quantile_table <- (data.frame(
          Quantile = names(quantiles),
          Value = as.numeric(quantiles)
        ))
        
      }
    }, 
    digits = 9,
    caption = "Maximum IRR Values",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # create 95% interval
    
    output$interval_data_s18 <- renderTable({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        dflow <- analysis_results_18()[analysis_results_18()$PNC == min(analysis_results_18()$PNC),"IRR"]
        dfhigh <- analysis_results_18()[analysis_results_18()$PNC == max(analysis_results_18()$PNC),"IRR"]
        
        quantile_table <- data.frame("PNC" = c(min(analysis_results_18()$PNC),max(analysis_results_18()$PNC)),
                                     "0.025" = c(quantile(dflow,0.025),quantile(dfhigh,0.025)),
                                     "0.975" = c(quantile(dflow,0.975),quantile(dfhigh,0.975))
        )
        return(quantile_table)
        
      }
    }, 
    digits = 9, 
    caption = "95% Interval",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    # create ecdf plot
    
    output$ecdf_plot_s18 <- renderPlot({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        dflow <- analysis_results_18()[analysis_results_18()$PNC == min(analysis_results_18()$PNC),"IRR"]
        dfhigh <- analysis_results_18()[analysis_results_18()$PNC == max(analysis_results_18()$PNC),"IRR"]
        
        fig <-ggplot() +
          stat_ecdf(aes(dflow), color = "darkblue") +
          stat_ecdf(aes(dfhigh), color = "red") +
          
          #Point LL
          geom_segment(aes(x = quantile(dflow,0.025), y = 0.025-0.02, xend = quantile(dflow,0.025), yend = 0.025+0.02)) +
          geom_segment(aes(x = (quantile(dflow,0.025)-0.005), y = 0.025, xend = (quantile(dflow,0.025)+0.005), yend = 0.025)) +
          annotate("text", x=quantile(dflow,0.025)-0.010, y=0.025+0.030, label= round(quantile(dflow,0.025),3)) +
          
          #Point RL
          geom_segment(aes(x = quantile(dfhigh,0.025), y = 0.025-0.02, xend = quantile(dfhigh,0.025), yend = 0.025+0.02)) +
          geom_segment(aes(x = (quantile(dfhigh,0.025)-0.005), y = 0.025, xend = (quantile(dfhigh,0.025)+0.005), yend = 0.025)) +
          annotate("text", x=quantile(dfhigh,0.025)+0.005, y=0.025+0.030, label= round(quantile(dfhigh,0.025),3)) +
          
          # Point LH
          geom_segment(aes(x = quantile(dflow,0.975), y = 0.975-0.02, xend = quantile(dflow,0.975), yend = 0.975+0.02)) +
          geom_segment(aes(x = (quantile(dflow,0.975)-0.005), y = 0.975, xend = (quantile(dflow,0.975)+0.005), yend = 0.975)) +
          annotate("text", x=quantile(dflow,0.975)-0.010, y=0.975-0.030, label= round(quantile(dflow,0.975),3)) +
          
          # Point RL
          geom_segment(aes(x = quantile(dfhigh,0.975), y = 0.975-0.02, xend = quantile(dfhigh,0.975), yend = 0.975+0.02)) +
          geom_segment(aes(x = (quantile(dfhigh,0.975)-0.005), y = 0.975, xend = (quantile(dfhigh,0.975)+0.005), yend = 0.975)) +
          annotate("text", x=quantile(dfhigh,0.975)-0.010, y=0.975-0.030, label= round(quantile(dfhigh,0.975),3)) +
          
          ggtitle("ECDF for IRR from Simulation (Cumulative Density Function)") +
          xlab("IRR") +
          ylab("Probability")
        
        fig
        
      }
    })
    
    # create uncertainty statements
    
    output$uncertainty_statements_s18 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[4])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #f0f8ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #4CAF50;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[8])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #f0f8ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #4CAF50;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[9])
        )
        
        # Return all three div elements
        tagList(div1, div2, div3)
      }
    })  
    
    # add summary comment
    
    output$summary_comment_s18 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[18])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Recommendations
    
    output$recommendation_statement_s18 <- renderUI({
      
      if(input$eType_PNC == 'epistemicUnc_PNC' & input$eType_PC == 'aleatoricUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          p(recommendation_list[1])
        )
        
        tagList(div1)
      }
    })  
    
    ## End System 18                                ##
    
    ## System 19                                    ##
    ## PNC = Aleatoric              ##
    ## PC = Point                   ##
    ## PTARG = Point                ##
    
    ## Create Subtitle
    
    output$subtitle_s19 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is an ",type_list[3], ", PC is a ", type_list[1], ", Target is a ",type_list[1]))
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    }) 
    
    ## Create data
    
    analysis_results_19 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
      # Generate Input Vectors
      
      if(input$PNC_in == "PNC_norm"){
        # Normal distribution
        PNC_mean <- isolate(input$PF_NC_mean)
        PNC_sd <- isolate(input$PF_NC_sd)
        PNC <- rnorm(100000, mean = PNC_mean, sd = PNC_sd)
      } 
      else if(input$NC_in == "PNC_beta"){
        PNC_shape1 <- isolate(input$PF_NC_shape1)
        PNC_shape2 <- isolate(input$PF_NC_shape2)
        PNC <- rbeta(100000, shape1 = PNC_shape1, shape2 = PNC_shape2)
      }
      else if(input$PNC_in == "PNC_lgnorm"){
        PNC_lgMean <- isolate(input$PF_NC_lgMean)
        PNC_lgSD <- isolate(input$PF_NC_lgSD)
        PNC <- rlnorm(100000, meanlog = PNC_lgMean, sdlog = PNC_lgSD)
      }
      
      PC <- rep(input$PF_C, times = 100000)
      PoF <- rep(input$PF_TARG, times = 100000)
      
      # Create dataframe
      
      df <- data.frame("PNC" = PNC, "PC" = PC, "PoF" = PoF)
      
      # Add IRR
      
      df$IRR <- round((df$PNC - df$PoF)/(df$PNC - df$PC),digits = 9)
      
      # Check constraints
      
      df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
      
      df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
      
      return(df)
      
      }
    })
    
    # create results histogram
    
    output$aleatoric_hist_s19 <- renderPlotly({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
      fig <- plot_ly(type = "histogram", alpha = 0.8) 
      
      fig <- fig %>% add_histogram(x = ~analysis_results_19()$IRR, name = "PC and Target PoF Are Point Estimates")
      
      fig <- fig %>%
        layout(
          xaxis = list(title = "IRR Distribution"),
          yaxis = list(title = "Frequency")
        )
      
      fig <- fig %>% config(
        displayModeBar = TRUE,  # Show the toolbar
        modeBarButtonsToRemove = c(
          'select2d',
          'lasso2d',
          'autoScale2d',
          'hoverClosestCartesian',
          'hoverCompareCartesian',
          'toggleSpikelines'
        ),
        displaylogo = FALSE,  # Remove plotly logo
        toImageButtonOptions = list(
          format = 'png',
          filename = 'IRR_plot',
          height = 600,
          width = 800,
          scale = 1
        )
      )
      
      fig
      
      }
    })
    
    #create measure of utilization
    
    output$count_data_s19 <- renderPrint({

      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
      value <- nrow(analysis_results_19())/100000
      paste("Fraction of simulated values included in the histogram", value)
      
      }
    })
    
    # add summary comment
    
    output$results_comment_s19 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[19])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Summary
    
    # create summarization table
    
    output$quantile_data_s19 <- renderTable({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        quantiles <- round(quantile(analysis_results_19()$IRR, probs = c(0.95, 0.975, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999, 0.99999999, 0.999999999)),digits = 9)
        quantile_table <- (data.frame(
          Quantile = names(quantiles),
          Value = as.numeric(quantiles)
        ))
        
      }
    }, digits = 9, 
    caption = "Maximum IRR Values",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # create 95% interval
    
    output$interval_data_s19 <- renderTable({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        quantiles <- round(quantile(analysis_results_19()$IRR, probs = c(0.025, 0.975)),digits = 9)
        quantile_table <- (data.frame(
          Quantile = names(quantiles),
          Value = as.numeric(quantiles)
        ))
        
      }
    }, digits = 9, caption = "95% Interval",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    # create ecdf plot
    
    output$ecdf_plot_s19 <- renderPlot({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        fig <- ggplot(analysis_results_19(),aes(x = IRR))+
          stat_ecdf() +
          ggtitle("ECDF for IRR from Simulation (Cumulative Density Function)")
        fig
        
      }
    })
    
    # create uncertainty statements
    
    output$uncertainty_statements_s19 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f0f8ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #4CAF50;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[7])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[2])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[3])
        )
        
        # Return all three div elements
        tagList(div1, div2, div3)
      }
    })  
    
    # add summary comment
    
    output$summary_comment_s19 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[19])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Recommendations
    
    output$recommendation_statement_s19 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          p(recommendation_list[1])
        )
        
        tagList(div1)
      }
    })  
    
    ## End System 19                ##                               
    
    ## System 20                    ##
    ## PNC = Aleatoric              ##
    ## PC = Point                   ##
    ## PTARG = Epistemic            ##
    
    ## Create Subtitle
    
    output$subtitle_s20 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is an ",type_list[3], ", PC is a ", type_list[1], ", Target is a ",type_list[1]))
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    }) 
    
    ## Create data
    
    analysis_results_20 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
      # Generate Input Vectors
      
        if(input$PNC_in == "PNC_norm"){
          # Normal distribution
          PNC_mean <- isolate(input$PF_NC_mean)
          PNC_sd <- isolate(input$PF_NC_sd)
          PNC <- rnorm(100000, mean = PNC_mean, sd = PNC_sd)
        } 
        else if(input$NC_in == "PNC_beta"){
          PNC_shape1 <- isolate(input$PF_NC_shape1)
          PNC_shape2 <- isolate(input$PF_NC_shape2)
          PNC <- rbeta(100000, shape1 = PNC_shape1, shape2 = PNC_shape2)
        }
        else if(input$PNC_in == "PNC_lgnorm"){
          PNC_lgMean <- isolate(input$PF_NC_lgMean)
          PNC_lgSD <- isolate(input$PF_NC_lgSD)
          PNC <- rlnorm(100000, meanlog = PNC_lgMean, sdlog = PNC_lgSD)
        }
        
      PC <- isolate(input$PF_C)
      
      PoF_lower <- isolate(input$PF_TARG_lower)
      PoF_upper <- isolate(input$PF_TARG_upper)
      PoF <- c(PoF_lower,PoF_upper)
      
      # Create dataframe
      
      df <- data.frame("PNC" = PNC, "PC" = rep(PC, times = 100000), "PoF" = rep(PoF, times = 50000))
      
      # Add IRR
      
      df$IRR <- round((df$PNC - df$PoF)/(df$PNC - df$PC),digits = 9)
      
      # Check Constraints
      
      df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
      
      df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
      
      return(df)
      
      }
    })
    
    # create results histogram
    
    output$aleatoric_hist_s20 <- renderPlotly({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
      fig <- plot_ly(type = "histogram", alpha = 0.8) 
      
      fig <- fig %>% add_histogram(x = ~analysis_results_20()[analysis_results_20()$PoF == input$PF_TARG_lower,"IRR"], name = "Target lower")
      
      fig <- fig %>% add_histogram(x = ~analysis_results_20()[analysis_results_20()$PoF == input$PF_TARG_upper,"IRR"], name = "Target upper")
      
      fig <- fig %>%
        layout(
          xaxis = list(title = "IRR Distribution"),
          yaxis = list(title = "Frequency",
                       barmode = "overlay")
        )
      
      fig <- fig %>% config(
        displayModeBar = TRUE,  # Show the toolbar
        modeBarButtonsToRemove = c(
          'select2d',
          'lasso2d',
          'autoScale2d',
          'hoverClosestCartesian',
          'hoverCompareCartesian',
          'toggleSpikelines'
        ),
        displaylogo = FALSE,  # Remove plotly logo
        toImageButtonOptions = list(
          format = 'png',
          filename = 'IRR_plot',
          height = 600,
          width = 800,
          scale = 1
        )
      )
      
      fig
      
      }
    })
    
    #create measure of utilization
    
    output$count_data_s20 <- renderPrint({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        value <- nrow(analysis_results_20())/100000
      paste("Fraction of simulated values included in the histogram", value)
      
      }
    })
    
    
    # add results comment
    
    output$results_comment_s20 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[20])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Summary
    
    # create summarization table
    
    output$quantile_data_s20 <- renderTable({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        quantiles <- round(quantile(analysis_results_20()$IRR, probs = c(0.95, 0.975, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999, 0.99999999, 0.999999999)),digits = 9)
        quantile_table <- (data.frame(
          Quantile = names(quantiles),
          Value = as.numeric(quantiles)
        ))
        
      }
    }, 
    digits = 9,
    caption = "Maximum IRR Values",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # create 95% interval
    
    output$interval_data_s20 <- renderTable({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        dflow <- analysis_results_20()[analysis_results_20()$PoF == min(analysis_results_20()$PoF),"IRR"]
        dfhigh <- analysis_results_20()[analysis_results_20()$PoF == max(analysis_results_20()$PoF),"IRR"]
        
        quantile_table <- data.frame("PoF" = c(min(analysis_results_20()$PoF),max(analysis_results_20()$PoF)),
                                     "0.025" = c(quantile(dflow,0.025),quantile(dfhigh,0.025)),
                                     "0.975" = c(quantile(dflow,0.975),quantile(dfhigh,0.975))
        )
        return(quantile_table)
        
      }
    }, 
    digits = 9, 
    caption = "95% Interval",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    # create ecdf plot
    
    output$ecdf_plot_s20 <- renderPlot({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        dflow <- analysis_results_20()[analysis_results_20()$PoF == min(analysis_results_20()$PoF),"IRR"]
        dfhigh <- analysis_results_20()[analysis_results_20()$PoF == max(analysis_results_20()$PoF),"IRR"]
        
        fig <-ggplot() +
          stat_ecdf(aes(dflow), color = "darkblue") +
          stat_ecdf(aes(dfhigh), color = "red") +
          
          #Point LL
          geom_segment(aes(x = quantile(dflow,0.025), y = 0.025-0.02, xend = quantile(dflow,0.025), yend = 0.025+0.02)) +
          geom_segment(aes(x = (quantile(dflow,0.025)-0.005), y = 0.025, xend = (quantile(dflow,0.025)+0.005), yend = 0.025)) +
          annotate("text", x=quantile(dflow,0.025)-0.010, y=0.025+0.030, label= round(quantile(dflow,0.025),3)) +
          
          #Point RL
          geom_segment(aes(x = quantile(dfhigh,0.025), y = 0.025-0.02, xend = quantile(dfhigh,0.025), yend = 0.025+0.02)) +
          geom_segment(aes(x = (quantile(dfhigh,0.025)-0.005), y = 0.025, xend = (quantile(dfhigh,0.025)+0.005), yend = 0.025)) +
          annotate("text", x=quantile(dfhigh,0.025)+0.005, y=0.025+0.030, label= round(quantile(dfhigh,0.025),3)) +
          
          # Point LH
          geom_segment(aes(x = quantile(dflow,0.975), y = 0.975-0.02, xend = quantile(dflow,0.975), yend = 0.975+0.02)) +
          geom_segment(aes(x = (quantile(dflow,0.975)-0.005), y = 0.975, xend = (quantile(dflow,0.975)+0.005), yend = 0.975)) +
          annotate("text", x=quantile(dflow,0.975)-0.010, y=0.975-0.030, label= round(quantile(dflow,0.975),3)) +
          
          # Point RL
          geom_segment(aes(x = quantile(dfhigh,0.975), y = 0.975-0.02, xend = quantile(dfhigh,0.975), yend = 0.975+0.02)) +
          geom_segment(aes(x = (quantile(dfhigh,0.975)-0.005), y = 0.975, xend = (quantile(dfhigh,0.975)+0.005), yend = 0.975)) +
          annotate("text", x=quantile(dfhigh,0.975)-0.010, y=0.975-0.030, label= round(quantile(dfhigh,0.975),3)) +
          
          ggtitle("ECDF for IRR from Simulation (Cumulative Density Function)") +
          xlab("IRR") +
          ylab("Probability")
        
        fig
        
      }
    })
    
    # create uncertainty statements
    
    output$uncertainty_statements_s20 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f0f8ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #4CAF50;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[7])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[2])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[6])
        )
        
        # Return all three div elements
        tagList(div1, div2, div3)
      }
    })  
    
    # add summary comment
    
    output$summary_comment_s20 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[20])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Recommendations
    
    output$recommendation_statement_s20 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          p(recommendation_list[1])
        )
        
        tagList(div1)
      }
    })  
    
    ## End System 20                 ##
    
    ## System 21  ##
    ## PNC = Aleatoric              ##
    ## PC = Point                   ##
    ## PTARG = Aleatoric            ##
    
    ## Create Subtitle
    
    output$subtitle_s21 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is an ",type_list[3], ", PC is a ", type_list[1], ", Target is a ",type_list[3]))
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    }) 
    
    ## Create data
    
    analysis_results_21 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
      # Create Vectors
      
        if(input$PNC_in == "PNC_norm"){
          # Normal distribution
          PNC_mean <- isolate(input$PF_NC_mean)
          PNC_sd <- isolate(input$PF_NC_sd)
          PNC <- rnorm(100000, mean = PNC_mean, sd = PNC_sd)
        } 
        else if(input$NC_in == "PNC_beta"){
          PNC_shape1 <- isolate(input$PF_NC_shape1)
          PNC_shape2 <- isolate(input$PF_NC_shape2)
          PNC <- rbeta(100000, shape1 = PNC_shape1, shape2 = PNC_shape2)
        }
        else if(input$PNC_in == "PNC_lgnorm"){
          PNC_lgMean <- isolate(input$PF_NC_lgMean)
          PNC_lgSD <- isolate(input$PF_NC_lgSD)
          PNC <- rlnorm(100000, meanlog = PNC_lgMean, sdlog = PNC_lgSD)
        }
        
      PC <- isolate(input$PF_C)
      
      if(input$TARG_in == "TARG_norm"){
        # Normal distribution
        PTARG_mean <- isolate(input$PF_TARG_mean)
        PTARG_sd <- isolate(input$PF_TARG_sd)
        PoF <- rnorm(100000, mean = PTARG_mean, sd = PTARG_sd)
      } 
      else if(input$TARG_in == "TARG_beta"){
        PTARG_shape1 <- isolate(input$PF_TARG_shape1)
        PTARG_shape2 <- isolate(input$PF_TARG_shape2)
        PoF <- rbeta(100000, shape1 = PTARG_shape1, shape2 = PTARG_shape2)
      }
      else if(input$TARG_in == "TARG_lgnorm"){
        PTARG_lgMmean <- isolate(input$PF_TARG_lgMean)
        PTARG_lgSD <- isolate(input$PF_TARG_lgSD)
        PoF <- rlnorm(100000, meanlog = PTARG_lgMean, sdlog = PTARG_lgSD)
      }
      
      # Create dataframe
      
      df <- data.frame("PNC" = PNC, "PC" = rep(PC, times = 100000), "PoF" = PoF)
      
      # Add IRR
      
      df$IRR <- round((df$PNC - df$PoF)/(df$PNC - df$PC),digits = 9)
      
      # Check Constraints
      
      df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
      
      df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
      
      return(df)
      
      }
    })
    
    # create results histogram
    
    output$aleatoric_hist_s21 <- renderPlotly({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        fig <- plot_ly(type = "histogram", alpha = 0.8) 
      
      fig <- fig %>% add_histogram(x = ~analysis_results_21()$IRR, name = "PC is a point estimate")
      
      fig <- fig %>%
        layout(
          xaxis = list(title = "IRR Distribution"),
          yaxis = list(title = "Frequency")
        )
      
      fig <- fig %>% config(
        displayModeBar = TRUE,  # Show the toolbar
        modeBarButtonsToRemove = c(
          'select2d',
          'lasso2d',
          'autoScale2d',
          'hoverClosestCartesian',
          'hoverCompareCartesian',
          'toggleSpikelines'
        ),
        displaylogo = FALSE,  # Remove plotly logo
        toImageButtonOptions = list(
          format = 'png',
          filename = 'IRR_plot',
          height = 600,
          width = 800,
          scale = 1
        )
      )
      
      fig
      
      }
    })
    
    #create measure of utilization
    
    output$count_data_s21 <- renderPrint({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        value <- nrow(analysis_results_21())/100000
      paste("Fraction of simulated values included in the histogram", value)
      
      }
    })
    
    # add summary comment
    
    output$results_comment_s21 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[21])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Summary
    
    # create summarization table
    
    output$quantile_data_s21 <- renderTable({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        quantiles <- round(quantile(analysis_results_21()$IRR, probs = c(0.95, 0.975, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999, 0.99999999, 0.999999999)),digits = 9)
        quantile_table <- (data.frame(
          Quantile = names(quantiles),
          Value = as.numeric(quantiles)
        ))
        
      }
    }, digits = 9, 
    caption = "Maximum IRR Values",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # create 95% interval
    
    output$interval_data_s21 <- renderTable({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        quantiles <- round(quantile(analysis_results_21()$IRR, probs = c(0.025, 0.975)),digits = 9)
        quantile_table <- (data.frame(
          Quantile = names(quantiles),
          Value = as.numeric(quantiles)
        ))
        
      }
    }, digits = 9, caption = "95% Interval",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    # create ecdf plot
    
    output$ecdf_plot_s21 <- renderPlot({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        
        fig <- ggplot(analysis_results_21(),aes(x = IRR))+
          stat_ecdf() +
          ggtitle("ECDF for IRR from Simulation (Cumulative Density Function)")
        fig
        
      }
    })
    
    # create uncertainty statements
    
    output$uncertainty_statements_s21 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f0f8ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #4CAF50;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[7])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[2])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #f0f8ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #4CAF50;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[9])
        )
        
        # Return all three div elements
        tagList(div1, div2, div3)
      }
    })  
    
    # add summary comment
    
    output$summary_comment_s21 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[21])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Recommendations
    
    output$recommendation_statement_s21 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'aleatoricUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          p(recommendation_list[1])
        )
        
        tagList(div1)
      }
    })  
    
    ## End System 21                                 ##
    
    ## System 22  ##
    ## PNC = Aleatoric              ##
    ## PC = Epistemic                   ##
    ## PTARG = Point            ##
    
    ## Create Subtitle
    
    output$subtitle_s22 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is an ",type_list[3], ", PC is a ", type_list[2], ", Target is a ",type_list[1]))
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    }) 
    
    ## Create data
    
    analysis_results_22 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
      # Create Vectors
      
        if(input$PNC_in == "PNC_norm"){
          # Normal distribution
          PNC_mean <- isolate(input$PF_NC_mean)
          PNC_sd <- isolate(input$PF_NC_sd)
          PNC <- rnorm(100000, mean = PNC_mean, sd = PNC_sd)
        } 
        else if(input$NC_in == "PNC_beta"){
          PNC_shape1 <- isolate(input$PF_NC_shape1)
          PNC_shape2 <- isolate(input$PF_NC_shape2)
          PNC <- rbeta(100000, shape1 = PNC_shape1, shape2 = PNC_shape2)
        }
        else if(input$PNC_in == "PNC_lgnorm"){
          PNC_lgMean <- isolate(input$PF_NC_lgMean)
          PNC_lgSD <- isolate(input$PF_NC_lgSD)
          PNC <- rlnorm(100000, meanlog = PNC_lgMean, sdlog = PNC_lgSD)
        }
        
      PC_lower <- isolate(input$PF_C_lower)
      PC_upper <- isolate(input$PF_C_upper)
      PC <- c(PC_lower, PC_upper)
      
      PoF <- isolate(input$PF_TARG)
      
      # Create dataframe
      
      df <- data.frame("PNC" = PNC, "PC" = rep(PC, times = 100000), "PoF" = rep(PoF, times = 100000))
      
      # Add IRR
      
      df$IRR <- round((df$PNC - df$PoF)/(df$PNC - df$PC),digits = 9)
      
      # Check Constraints
      
      df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
      
      df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
      
      return(df)
      
      }
    })
    
    # create results histogram
    
    output$aleatoric_hist_s22 <- renderPlotly({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
      fig <- plot_ly(type = "histogram", alpha = 0.8) 
      
      fig <- fig %>% add_histogram(x = ~analysis_results_22()[analysis_results_22()$PC == input$PF_C_lower,"IRR"], name = "PC lower")
      
      fig <- fig %>% add_histogram(x = ~analysis_results_22()[analysis_results_22()$PC == input$PF_C_upper,"IRR"], name = "PC upper")
      
      fig <- fig %>%
        layout(
          xaxis = list(title = "IRR Distribution"),
          yaxis = list(title = "Frequency",
                       barmode = "overlay")
        )
      
      fig <- fig %>% config(
        displayModeBar = TRUE,  # Show the toolbar
        modeBarButtonsToRemove = c(
          'select2d',
          'lasso2d',
          'autoScale2d',
          'hoverClosestCartesian',
          'hoverCompareCartesian',
          'toggleSpikelines'
        ),
        displaylogo = FALSE,  # Remove plotly logo
        toImageButtonOptions = list(
          format = 'png',
          filename = 'IRR_plot',
          height = 600,
          width = 800,
          scale = 1
        )
      )
      
      fig
      
      }
    })
    
    #create measure of utilization
    
    output$count_data_s22 <- renderPrint({

      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        value <- nrow(analysis_results_22())/100000
      paste("Fraction of simulated values included in the histogram", value)
      
      }
    })
    
    # add results comment
    
    output$results_comment_s22 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'unknownUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[22])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Summary
    
    # create summarization table
    
    output$quantile_data_s22 <- renderTable({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        quantiles <- round(quantile(analysis_results_22()$IRR, probs = c(0.95, 0.975, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999, 0.99999999, 0.999999999)),digits = 9)
        quantile_table <- (data.frame(
          Quantile = names(quantiles),
          Value = as.numeric(quantiles)
        ))
        
      }
    }, 
    digits = 9,
    caption = "Maximum IRR Values",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # create 95% interval
    
    output$interval_data_s22 <- renderTable({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        dflow <- analysis_results_22()[analysis_results_22()$PC == min(analysis_results_22()$PC),"IRR"]
        dfhigh <- analysis_results_22()[analysis_results_22()$PC == max(analysis_results_22()$PC),"IRR"]
        
        quantile_table <- data.frame("PC" = c(min(analysis_results_22()$PC),max(analysis_results_22()$PC)),
                                     "0.025" = c(quantile(dflow,0.025),quantile(dfhigh,0.025)),
                                     "0.975" = c(quantile(dflow,0.975),quantile(dfhigh,0.975))
        )
        return(quantile_table)
        
      }
    }, 
    digits = 9, 
    caption = "95% Interval",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    # create ecdf plot
    
    output$ecdf_plot_s22 <- renderPlot({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        
        dflow <- analysis_results_22()[analysis_results_22()$PC == min(analysis_results_22()$PC),"IRR"]
        dfhigh <- analysis_results_22()[analysis_results_22()$PC == max(analysis_results_22()$PC),"IRR"]
        
        fig <-ggplot() +
          stat_ecdf(aes(dflow), color = "darkblue") +
          stat_ecdf(aes(dfhigh), color = "red") +
          
          #Point LL
          geom_segment(aes(x = quantile(dflow,0.025), y = 0.025-0.02, xend = quantile(dflow,0.025), yend = 0.025+0.02)) +
          geom_segment(aes(x = (quantile(dflow,0.025)-0.005), y = 0.025, xend = (quantile(dflow,0.025)+0.005), yend = 0.025)) +
          annotate("text", x=quantile(dflow,0.025)-0.010, y=0.025+0.030, label= round(quantile(dflow,0.025),3)) +
          
          #Point RL
          geom_segment(aes(x = quantile(dfhigh,0.025), y = 0.025-0.02, xend = quantile(dfhigh,0.025), yend = 0.025+0.02)) +
          geom_segment(aes(x = (quantile(dfhigh,0.025)-0.005), y = 0.025, xend = (quantile(dfhigh,0.025)+0.005), yend = 0.025)) +
          annotate("text", x=quantile(dfhigh,0.025)+0.005, y=0.025+0.030, label= round(quantile(dfhigh,0.025),3)) +
          
          # Point LH
          geom_segment(aes(x = quantile(dflow,0.975), y = 0.975-0.02, xend = quantile(dflow,0.975), yend = 0.975+0.02)) +
          geom_segment(aes(x = (quantile(dflow,0.975)-0.005), y = 0.975, xend = (quantile(dflow,0.975)+0.005), yend = 0.975)) +
          annotate("text", x=quantile(dflow,0.975)-0.010, y=0.975-0.030, label= round(quantile(dflow,0.975),3)) +
          
          # Point RL
          geom_segment(aes(x = quantile(dfhigh,0.975), y = 0.975-0.02, xend = quantile(dfhigh,0.975), yend = 0.975+0.02)) +
          geom_segment(aes(x = (quantile(dfhigh,0.975)-0.005), y = 0.975, xend = (quantile(dfhigh,0.975)+0.005), yend = 0.975)) +
          annotate("text", x=quantile(dfhigh,0.975)-0.010, y=0.975-0.030, label= round(quantile(dfhigh,0.975),3)) +
          
          ggtitle("ECDF for IRR from Simulation (Cumulative Density Function)") +
          xlab("IRR") +
          ylab("Probability")
        
        fig
        
      }
    })
    
    # create uncertainty statements
    
    output$uncertainty_statements_s22 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f0f8ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #4CAF50;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[7])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[5])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[3])
        )
        
        # Return all three div elements
        tagList(div1, div2, div3)
      }
    })  
    
    # add summary comment
    
    output$summary_comment_s22 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[22])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Recommendations
    
    output$recommendation_statement_s22 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'unknownUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f8f0ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #9C27B0;",
          p(recommendation_list[1])
        )
        
        tagList(div1)
      }
    })  
    
    ## End System 22                 ##
    
    ## System 23  ##
    ## PNC = Aleatoric              ##
    ## PC = Epistemic               ##
    ## PTARG = Epistemic            ##
    
    ## Create Subtitle
    
    output$subtitle_s23 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        # Create div for Variable 1 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(paste("PNC is an ",type_list[3], ", PC is a ", type_list[2], ", Target is a ",type_list[2]))
        )
        
        # Return all div elements
        
        tagList(div1)
      }
    }) 
    
    ## Create data
    
    analysis_results_23 <- eventReactive(input$start_btn, {
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create Vectors
      
        if(input$PNC_in == "PNC_norm"){
          # Normal distribution
          PNC_mean <- isolate(input$PF_NC_mean)
          PNC_sd <- isolate(input$PF_NC_sd)
          PNC <- rnorm(100000, mean = PNC_mean, sd = PNC_sd)
        } 
        else if(input$NC_in == "PNC_beta"){
          PNC_shape1 <- isolate(input$PF_NC_shape1)
          PNC_shape2 <- isolate(input$PF_NC_shape2)
          PNC <- rbeta(100000, shape1 = PNC_shape1, shape2 = PNC_shape2)
        }
        else if(input$PNC_in == "PNC_lgnorm"){
          PNC_lgMean <- isolate(input$PF_NC_lgMean)
          PNC_lgSD <- isolate(input$PF_NC_lgSD)
          PNC <- rlnorm(100000, meanlog = PNC_lgMean, sdlog = PNC_lgSD)
        }
        
      PC_lower <- isolate(input$PF_C_lower)
      PC_upper <- isolate(input$PF_C_upper)
      PC <- c(PC_lower, PC_upper)
      
      PoF_lower <- isolate(input$PF_TARG_lower)
      PoF_upper <- isolate(input$PF_TARG_upper)
      PoF <- c(PoF_lower, PoF_upper)
      
      # Create dataframe
      
      test <- expand.grid(PC,PoF)
      
      df <- data.frame("PNC" = PNC, "PC" = rep(test$Var1,25000),"PoF" = rep(test$Var2,25000))
      
      # Add IRR
      
      df$IRR <- round((df$PNC - df$PoF)/(df$PNC - df$PC),digits = 9)
      
      # Check Constraints
      
      df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
      
      df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
      
      return(df)
      
      }
    })
    
    # create results histogram
    
    output$aleatoric_hist_s23 <- renderPlotly({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
      fig <- plot_ly(type = "histogram", alpha = 1.0) 
      
      fig <- fig %>% add_histogram(x = ~analysis_results_23()[analysis_results_23()$PC == input$PF_C_lower & analysis_results_23()$PoF == input$PF_TARG_lower,"IRR"], name = "PC lower & PoF lower")
      
      fig <- fig %>% add_histogram(x = ~analysis_results_23()[analysis_results_23()$PC == input$PF_C_upper & analysis_results_23()$PoF == input$PF_TARG_lower,"IRR"], name = "PC upper & PoF lower")
      
      fig <- fig %>% add_histogram(x = ~analysis_results_23()[analysis_results_23()$PC == input$PF_C_lower & analysis_results_23()$PoF == input$PF_TARG_upper,"IRR"], name = "PC lower & PoF upper")
      
      fig <- fig %>% add_histogram(x = ~analysis_results_23()[analysis_results_23()$PC == input$PF_C_upper & analysis_results_23()$PoF == input$PF_TARG_upper,"IRR"], name = "PC upper & PoF upper")
      
      fig <- fig %>%
        layout(
          xaxis = list(title = "IRR Distribution"),
          yaxis = list(title = "Frequency",
                       barmode = "overlay")
        )
      
      fig <- fig %>% config(
        displayModeBar = TRUE,  # Show the toolbar
        modeBarButtonsToRemove = c(
          'select2d',
          'lasso2d',
          'autoScale2d',
          'hoverClosestCartesian',
          'hoverCompareCartesian',
          'toggleSpikelines'
        ),
        displaylogo = FALSE,  # Remove plotly logo
        toImageButtonOptions = list(
          format = 'png',
          filename = 'IRR_plot',
          height = 600,
          width = 800,
          scale = 1
        )
      )
      
      fig
      
      }
    })
    
    #create measure of utilization
    
    output$count_data_s23 <- renderPrint({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        value <- nrow(analysis_results_23())/100000
      paste("Fraction of simulated values included in the histogram", value)
      
      }
    })
    
    # add results comment
    
    output$results_comment_s23 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(results_comment_list[23])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Summary
    
    # create summarization table
    
    output$quantile_data_s23 <- renderTable({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        quantiles <- round(quantile(analysis_results_23()$IRR, probs = c(0.95, 0.975, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999, 0.99999999, 0.999999999)),digits = 9)
        quantile_table <- (data.frame(
          Quantile = names(quantiles),
          Value = as.numeric(quantiles)
        ))
        
      }
    }, 
    digits = 9,
    caption = "Maximum IRR Values",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # create 95% interval
    
    output$interval_data_s23 <- renderTable({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        PClowPoFlow <- analysis_results_23()[analysis_results_23()$PC == min(analysis_results_23()$PC) & analysis_results_23()$PoF == min(analysis_results_23()$PoF),"IRR"]
        PChighPoFlow <- analysis_results_23()[analysis_results_23()$PC == max(analysis_results_23()$PC) & analysis_results_23()$PoF == min(analysis_results_23()$PoF),"IRR"]
        PClowPoFhigh <- analysis_results_23()[analysis_results_23()$PC == min(analysis_results_23()$PC) & analysis_results_23()$PoF == max(analysis_results_23()$PoF),"IRR"]
        PChighPoFhigh <- analysis_results_23()[analysis_results_23()$PC == max(analysis_results_23()$PC) & analysis_results_23()$PoF == max(analysis_results_23()$PoF),"IRR"]
        
        
        quantile_table <- data.frame("PC" = c(min(analysis_results_23()$PC),min(analysis_results_23()$PC),max(analysis_results_23()$PC),max(analysis_results_23()$PC)),
                                     "PoF" = c(min(analysis_results_23()$PoF),max(analysis_results_23()$PoF),min(analysis_results_23()$PoF),max(analysis_results_23()$PoF)),
                                     "0.025" = c(quantile(PClowPoFlow,0.025),quantile(PChighPoFlow,0.025),quantile(PClowPoFhigh,0.025),quantile(PChighPoFhigh,0.025)),
                                     "0.975" = c(quantile(PClowPoFlow,0.975),quantile(PChighPoFlow,0.975),quantile(PClowPoFhigh,0.975),quantile(PChighPoFhigh,0.975))
        )
        return(quantile_table)
        
      }
    }, 
    digits = 9, 
    caption = "95% Interval",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    # create ecdf plot
    
    output$ecdf_plot_s23 <- renderPlot({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        
        PClowPoFlow <- analysis_results_23()[analysis_results_23()$PC == min(analysis_results_23()$PC) & analysis_results_23()$PoF == min(analysis_results_23()$PoF),"IRR"]
        PChighPoFlow <- analysis_results_23()[analysis_results_23()$PC == max(analysis_results_23()$PC) & analysis_results_23()$PoF == min(analysis_results_23()$PoF),"IRR"]
        PClowPoFhigh <- analysis_results_23()[analysis_results_23()$PC == min(analysis_results_23()$PC) & analysis_results_23()$PoF == max(analysis_results_23()$PoF),"IRR"]
        PChighPoFhigh <- analysis_results_23()[analysis_results_23()$PC == max(analysis_results_23()$PC) & analysis_results_23()$PoF == max(analysis_results_23()$PoF),"IRR"]
        
        # Combine into a data frame
        df <- data.frame(PClowPoFlow = PClowPoFlow, PChighPoFlow = PChighPoFlow, PClowPoFhigh = PClowPoFhigh, PChighPoFhigh = PChighPoFhigh)
        
        # Compute medians
        #medians <- sapply(df, median)
        lower_quantiles <- sapply(df, function(x) mean(quantile(x, probs = c(0.05, 0.10, 0.15))))
        
        
        # Get names of lowest and highest median vectors
        lowest_name <- names(which.min(lower_quantiles))
        highest_name <- names(which.max(lower_quantiles))
        
        # Convert to symbols for ggplot
        lowest_data <- df[[lowest_name]]
        highest_data <- df[[highest_name]]
        
        fig <-ggplot() +
          stat_ecdf(aes(PClowPoFlow), color = "orange") +
          stat_ecdf(aes(PChighPoFlow), color = "red") +
          stat_ecdf(aes(PClowPoFhigh), color = "green") +
          stat_ecdf(aes(PChighPoFhigh), color = "purple") +
          
          #Point LL
          geom_segment(aes(x = quantile(lowest_data,0.025), y = 0.025-0.02, xend = quantile(lowest_data,0.025), yend = 0.025+0.02)) +
          geom_segment(aes(x = (quantile(lowest_data,0.025)-0.005), y = 0.025, xend = (quantile(lowest_data,0.025)+0.005), yend = 0.025)) +
          annotate("text", x=quantile(lowest_data,0.025)-0.010, y=0.025+0.030, label= round(quantile(lowest_data,0.025),3)) +
          
          #Point RL
          geom_segment(aes(x = quantile(highest_data,0.025), y = 0.025-0.02, xend = quantile(highest_data,0.025), yend = 0.025+0.02)) +
          geom_segment(aes(x = (quantile(highest_data,0.025)-0.005), y = 0.025, xend = (quantile(highest_data,0.025)+0.005), yend = 0.025)) +
          annotate("text", x=quantile(highest_data,0.025)+0.005, y=0.025+0.030, label= round(quantile(highest_data,0.025),3)) +
          
          # Point LH
          geom_segment(aes(x = quantile(lowest_data,0.975), y = 0.975-0.02, xend = quantile(lowest_data,0.975), yend = 0.975+0.02)) +
          geom_segment(aes(x = (quantile(lowest_data,0.975)-0.005), y = 0.975, xend = (quantile(lowest_data,0.975)+0.005), yend = 0.975)) +
          annotate("text", x=quantile(lowest_data,0.975)-0.010, y=0.975-0.030, label= round(quantile(lowest_data,0.975),3)) +
          
          # Point RL
          geom_segment(aes(x = quantile(highest_data,0.975), y = 0.975-0.02, xend = quantile(highest_data,0.975), yend = 0.975+0.02)) +
          geom_segment(aes(x = (quantile(highest_data,0.975)-0.005), y = 0.975, xend = (quantile(highest_data,0.975)+0.005), yend = 0.975)) +
          annotate("text", x=quantile(highest_data,0.975)-0.010, y=0.975-0.030, label= round(quantile(highest_data,0.975),3)) +
          
          ggtitle("ECDF for IRR from Simulation (Cumulative Density Function)") +
          xlab("IRR") +
          ylab("Probability")
        
        fig
        
      }
    })
    
    # create uncertainty statements
    
    output$uncertainty_statements_s23 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #f0f8ff; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #4CAF50;",
          h5("PNC Uncertainty Notes:"),
          p(uncertainty_list[7])
        )
        
        # Create div for Variable 2 statement
        div2 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("PC Uncertainty Assessment:"),
          p(uncertainty_list[5])
        )
        
        # Create div for Variable 3 statement
        div3 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          h5("Target Uncertainty Assessment:"),
          p(uncertainty_list[6])
        )
        
        # Return all three div elements
        tagList(div1, div2, div3)
      }
    })  
    
    # add summary comment
    
    output$summary_comment_s23 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        #Create div for Variable 3 statement
        div1 <- div(
          style = "text-align: center; font-size: 20px; font-weight: bold;",
          p(summary_comment_list[23])
        )
        
        # Return all three div elements
        tagList(div1)
      }
    })
    
    ## Create Recommendations
    
    output$recommendation_statement_s23 <- renderUI({
      
      if(input$eType_PNC == 'aleatoricUnc_PNC' & input$eType_PC == 'epistemicUnc_PC' & input$eType_PTARG == 'epistemicUnc_PTARG') {
        # Create div for Variable 1 statement
        div1 <- div(
          style = "background-color: #fff8f0; padding: 15px; margin: 10px; border-radius: 5px; border-left: 4px solid #FF9800;",
          p(recommendation_list[2])
        )
        
        tagList(div1)
      }
    })  
    
    ## End System 23                 ##
    
    ## System 24  ##
    
    # create data
    
    aleatoric_data_s24 <- reactive({
      
      # Create Vectors
      
      if(input$PNC_in == "PNC_norm"){
        # Normal distribution
        PNC <- rnorm(100000, mean = input$PF_NC_mean, sd = input$PF_NC_sd)
      } 
      else if(input$PNC_in == "PNC_beta"){
        PNC <- rbeta(100000, shape1 = input$PF_NC_shape1, shape2 = input$PF_NC_shape2)
      }
      else if(input$PNC_in == "PNC_lgnorm"){
        PNC <- rlnorm(100000, meanlog = input$PF_NC_lgMean, sdlog = input$PF_NC_lgSD)
      }
      
      PC <- rep(c(input$PF_C_lower,input$PF_C_upper), times = 50000)
      
      if(input$TARG_in == "PC_norm"){
        # Normal distribution
        PoF <- rnorm(100000, mean = input$PF_TARG_mean, sd = input$PF_TARG_sd)
      } 
      else if(input$TARG_in == "PC_beta"){
        PoF <- rbeta(100000, shape1 = input$PF_TARG_shape1, shape2 = input$PF_TARG_shape2)
      }
      else if(input$TARG_in == "PC_lgnorm"){
        PoF <- rlnorm(100000, meanlog = input$PF_TARG_lgMean, sdlog = input$PF_TARG_lgSD)
      }
      
      # Create dataframe
      
      df <- data.frame("PNC" = PNC, "PC" = PC, "PoF" = PoF)
      
      # Add IRR
      
      df$IRR <- round((df$PNC - df$PoF)/(df$PNC - df$PC),digits = 9)
      
      # Check Constraints
      
      df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
      
      df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
      
      return(df)
      
    })
    
    # create results histogram
    
    output$aleatoric_hist_s24 <- renderPlotly({
      
      fig <- plot_ly(type = "histogram", alpha = 0.8) 
      
      fig <- fig %>% add_histogram(x = ~aleatoric_data_s24()[aleatoric_data_s24()$PC == input$PF_C_lower,"IRR"], name = "PC lower")
      fig <- fig %>% add_histogram(x = ~aleatoric_data_s24()[aleatoric_data_s24()$PC == input$PF_C_upper,"IRR"], name = "PC upper")
      
      fig <- fig %>%
        layout(
          xaxis = list(title = "IRR Distribution"),
          yaxis = list(title = "Frequency")
        )
      
      fig <- fig %>% config(
        displayModeBar = TRUE,  # Show the toolbar
        modeBarButtonsToRemove = c(
          'select2d',
          'lasso2d',
          'autoScale2d',
          'hoverClosestCartesian',
          'hoverCompareCartesian',
          'toggleSpikelines'
        ),
        displaylogo = FALSE,  # Remove plotly logo
        toImageButtonOptions = list(
          format = 'png',
          filename = 'IRR_plot',
          height = 600,
          width = 800,
          scale = 1
        )
      )
      
      fig
      
    })
    
    #create measure of utilization
    
    output$count_data_s24 <- renderPrint({
      value <- nrow(aleatoric_data_s24())/100000
      paste("Fraction of simulated values included in the histogram", value)
    })
    
    # create summarization table
    
    output$quantile_data_s24 <- renderTable({
      quantiles <- round(quantile(aleatoric_data_s24()$IRR, probs = c(0.95, 0.975, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999, 0.99999999, 0.999999999)),digits = 9)
      quantile_table <- t(data.frame(
        Quantile = names(quantiles),
        Value = as.numeric(quantiles)
      ))
    }, digits = 9)
    
    ##                                          ##
    
    ## System 25                                ##
    
    # create data
    
    aleatoric_data_s25 <- reactive({
      
      # Create Vectors
      
      if(input$PNC_in == "PNC_norm"){
        # Normal distribution
        PNC <- rnorm(100000, mean = input$PF_NC_mean, sd = input$PF_NC_sd)
      } 
      else if(input$PNC_in == "PNC_beta"){
        PNC <- rbeta(100000, shape1 = input$PF_NC_shape1, shape2 = input$PF_NC_shape2)
      }
      else if(input$PNC_in == "PNC_lgnorm"){
        PNC <- rlnorm(100000, meanlog = input$PF_NC_lgMean, sdlog = input$PF_NC_lgSD)
      }
      
      if(input$PC_in == "PC_norm"){
        # Normal distribution
        PC <- rnorm(100000, mean = input$PF_C_mean, sd = input$PF_C_sd)
      } 
      else if(input$PC_in == "PC_beta"){
        PC <- rbeta(100000, shape1 = input$PF_C_shape1, shape2 = input$PF_C_shape2)
      }
      else if(input$PC_in == "PC_lgnorm"){
        PC <- rlnorm(100000, meanlog = input$PF_C_lgMean, sdlog = input$PF_C_lgSD)
      }
      
      PoF <- rep(input$PF_TARG, times = 100000)
      
      # Create dataframe
      
      df <- data.frame("PNC" = PNC, "PC" = PC, "PoF" = PoF)
      
      # Add IRR
      
      df$IRR <- round((df$PNC - df$PoF)/(df$PNC - df$PC),digits = 9)
      
      # Check Constraints
      
      df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
      
      df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
      
      return(df)
      
    })
    
    # create results histogram
    
    output$aleatoric_hist_s25 <- renderPlotly({
      
      fig <- plot_ly(type = "histogram", alpha = 0.8) 
      
      fig <- fig %>% add_histogram(x = ~aleatoric_data_s9()$IRR, name = "PC is a Point Estimate")
      
      fig <- fig %>%
        layout(
          xaxis = list(title = "IRR Distribution"),
          yaxis = list(title = "Frequency")
        )
      
      fig <- fig %>% config(
        displayModeBar = TRUE,  # Show the toolbar
        modeBarButtonsToRemove = c(
          'select2d',
          'lasso2d',
          'autoScale2d',
          'hoverClosestCartesian',
          'hoverCompareCartesian',
          'toggleSpikelines'
        ),
        displaylogo = FALSE,  # Remove plotly logo
        toImageButtonOptions = list(
          format = 'png',
          filename = 'IRR_plot',
          height = 600,
          width = 800,
          scale = 1
        )
      )
      
      fig
      
    })
    
    #create measure of utilization
    
    output$count_data_s25 <- renderPrint({
      value <- nrow(aleatoric_data_s25())/100000
      paste("Fraction of simulated values included in the histogram", value)
    })
    
    # create summarization table
    
    output$quantile_data_s25 <- renderTable({
      quantiles <- round(quantile(aleatoric_data_s25()$IRR, probs = c(0.95, 0.975, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999, 0.99999999, 0.999999999)),digits = 9)
      quantile_table <- t(data.frame(
        Quantile = names(quantiles),
        Value = as.numeric(quantiles)
      ))
    }, digits = 9)
    
    ##                                 ##  
    
    ## System 26                       ##
    
    # create data
    
    aleatoric_data_s26 <- reactive({
      
      # Create Vectors
      
      if(input$PNC_in == "PNC_norm"){
        # Normal distribution
        PNC <- rnorm(100000, mean = input$PF_NC_mean, sd = input$PF_NC_sd)
      } 
      else if(input$PNC_in == "PNC_beta"){
        PNC <- rbeta(100000, shape1 = input$PF_NC_shape1, shape2 = input$PF_NC_shape2)
      }
      else if(input$PNC_in == "PNC_lgnorm"){
        PNC <- rlnorm(100000, meanlog = input$PF_NC_lgMean, sdlog = input$PF_NC_lgSD)
      }
      
      if(input$PC_in == "PC_norm"){
        # Normal distribution
        PC <- rnorm(100000, mean = input$PF_C_mean, sd = input$PF_C_sd)
      } 
      else if(input$PC_in == "PC_beta"){
        PC <- rbeta(100000, shape1 = input$PF_C_shape1, shape2 = input$PF_C_shape2)
      }
      else if(input$PC_in == "PC_lgnorm"){
        PC <- rlnorm(100000, meanlog = input$PF_C_lgMean, sdlog = input$PF_C_lgSD)
      }
      
      PoF <- rep(c(input$PF_TARG_lower,input$PF_TARG_upper), times = 50000)
      
      
      # Create dataframe
      
      df <- data.frame("PNC" = PNC, "PC" = PC, "PoF" = PoF)
      
      # Add IRR
      
      df$IRR <- round((df$PNC - df$PoF)/(df$PNC - df$PC),digits = 9)
      
      # Check Constraints
      
      df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
      
      df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
      
      return(df)
      
    })
    
    # create results histogram
    
    output$aleatoric_hist_s26 <- renderPlotly({
      
      fig <- plot_ly(type = "histogram", alpha = 0.8) 
      
      fig <- fig %>% add_histogram(x = ~aleatoric_data_s26()[aleatoric_data_s26()$PoF == input$PF_TARG_lower,"IRR"], name = "PoF lower")
      fig <- fig %>% add_histogram(x = ~aleatoric_data_s26()[aleatoric_data_s26()$PoF == input$PF_TARG_upper,"IRR"], name = "PoF upper")
      
      fig <- fig %>%
        layout(
          xaxis = list(title = "IRR Distribution"),
          yaxis = list(title = "Frequency")
        )
      
      fig <- fig %>% config(
        displayModeBar = TRUE,  # Show the toolbar
        modeBarButtonsToRemove = c(
          'select2d',
          'lasso2d',
          'autoScale2d',
          'hoverClosestCartesian',
          'hoverCompareCartesian',
          'toggleSpikelines'
        ),
        displaylogo = FALSE,  # Remove plotly logo
        toImageButtonOptions = list(
          format = 'png',
          filename = 'IRR_plot',
          height = 600,
          width = 800,
          scale = 1
        )
      )
      
      fig
      
    })
    
    #create measure of utilization
    
    output$count_data_s26 <- renderPrint({
      value <- nrow(aleatoric_data_s26())/100000
      paste("Fraction of simulated values included in the histogram", value)
    })
    
    # create summarization table
    
    output$quantile_data_s26 <- renderTable({
      quantiles <- round(quantile(aleatoric_data_s26()$IRR, probs = c(0.95, 0.975, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999, 0.99999999, 0.999999999)),digits = 9)
      quantile_table <- t(data.frame(
        Quantile = names(quantiles),
        Value = as.numeric(quantiles)
      ))
    }, digits = 9)
    
    ##                                          ##
    
    ## System 27                       ##
    
    # create data
    
    aleatoric_data_s27 <- reactive({
      
      # Create Vectors
      
      if(input$PNC_in == "PNC_norm"){
        # Normal distribution
        PNC <- rnorm(100000, mean = input$PF_NC_mean, sd = input$PF_NC_sd)
      } 
      else if(input$PNC_in == "PNC_beta"){
        PNC <- rbeta(100000, shape1 = input$PF_NC_shape1, shape2 = input$PF_NC_shape2)
      }
      else if(input$PNC_in == "PNC_lgnorm"){
        PNC <- rlnorm(100000, meanlog = input$PF_NC_lgMean, sdlog = input$PF_NC_lgSD)
      }
      
      if(input$PC_in == "PC_norm"){
        # Normal distribution
        PC <- rnorm(100000, mean = input$PF_C_mean, sd = input$PF_C_sd)
      } 
      else if(input$PC_in == "PC_beta"){
        PC <- rbeta(100000, shape1 = input$PF_C_shape1, shape2 = input$PF_C_shape2)
      }
      else if(input$PC_in == "PC_lgnorm"){
        PC <- rlnorm(100000, meanlog = input$PF_C_lgMean, sdlog = input$PF_C_lgSD)
      }
      
      if(input$TARG_in == "PTARG_norm"){
        # Normal distribution
        PoF <- rnorm(100000, mean = input$PF_TARG_mean, sd = input$PF_TARG_sd)
      } 
      else if(input$TARG_in == "PTARG_beta"){
        PoF <- rbeta(100000, shape1 = input$PF_TARG_shape1, shape2 = input$PF_TARG_shape2)
      }
      else if(input$TARG_in == "PTARG_lgnorm"){
        PoF <- rlnorm(100000, meanlog = input$PF_TARG_lgMean, sdlog = input$PF_TARG_lgSD)
      }
      
      # Create dataframe
      
      df <- data.frame("PNC" = PNC, "PC" = PC, "PoF" = PoF)
      
      # Add IRR
      
      df$IRR <- round((df$PNC - df$PoF)/(df$PNC - df$PC),digits = 9)
      
      # Check Constraints
      
      df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
      
      df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
      
      return(df)
      
    })
    
    # create results histogram
    
    output$aleatoric_hist_s27 <- renderPlotly({
      
      fig <- plot_ly(type = "histogram", alpha = 0.8) 
      
      fig <- fig %>% add_histogram(x = ~aleatoric_data_s27()$IRR, name = "All are Aleatoric")
      
      fig <- fig %>%
        layout(
          xaxis = list(title = "IRR Distribution"),
          yaxis = list(title = "Frequency")
        )
      
      fig <- fig %>% config(
        displayModeBar = TRUE,  # Show the toolbar
        modeBarButtonsToRemove = c(
          'select2d',
          'lasso2d',
          'autoScale2d',
          'hoverClosestCartesian',
          'hoverCompareCartesian',
          'toggleSpikelines'
        ),
        displaylogo = FALSE,  # Remove plotly logo
        toImageButtonOptions = list(
          format = 'png',
          filename = 'IRR_plot',
          height = 600,
          width = 800,
          scale = 1
        )
      )
      
      fig
      
    })
    
    #create measure of utilization
    
    output$count_data_s27 <- renderPrint({
      value <- nrow(aleatoric_data_s27())/100000
      paste("Fraction of simulated values included in the histogram", value)
    })
    
    # create summarization table
    
    output$quantile_data_s27 <- renderTable({
      quantiles <- round(quantile(aleatoric_data_s27()$IRR, probs = c(0.95, 0.975, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999, 0.99999999, 0.999999999)),digits = 9)
      quantile_table <- t(data.frame(
        Quantile = names(quantiles),
        Value = as.numeric(quantiles)
      ))
    }, digits = 9)
    
    ##                                          ##
  })
  
  # Beta Distribution
  
  # mean <- 0.7
  # 
  # #what I call "narrowness" is an invented, it will become the lower one of the beta/alpha value
  # #if you set narrowness higher it will narrow the pdf; below 1.5 it might lead to unintuitive output with maximum being super close to 1 or 0
  # narrowness <- 2 
  # 
  # #To calibrate narrowness to your liking.
  # #leave mean at 0.5
  # # mean<-0.5 
  # #set variance to whatever you want however if you go too high alpha/beta will become - unfinished sentence?
  # # var<-0.05 
  # #this is how you would calculate alpha/beta if mean is 0.5
  # # narrowness <- ((1 - mean) / var - 1 / mean) * mean ^ 2  
  # 
  # if (mean < 0.5) {
  #   alpha <- narrowness
  #   beta <- ((-alpha*mean)+alpha)/mean
  # } else {
  #   beta <- narrowness
  #   alpha <- (-beta*mean)/(mean-1)  
  # }
  # print(c(alpha,beta))
  # 
  # 
  # #if you want the mode/maximum to be e.g. 0.8, set mean to 0.8 and add 1 each to alpha and beta, however your mean is not gonna be 0.8 anymore, see below
  # distribution <- stats::rbeta(100000, alpha+1, beta+1, ncp = 0) #ncp = 0 is default and changing it will push the distribution in towards right/left, I have not tried it out
  # 
  # #if you want the mean to be 0.8 just leave as it is below and comment line above
  # #distribution<-stats::rbeta(numbers_drawn, alpha, beta, ncp = 0)
  # 
  # #var<-(mean^3-2*mean^2+mean)/(beta-mean+1)  #calculate var just from beta and mean
  # #calculate mode (most common number/maximum of the pdf)
  # dist <- round(distribution, digits = 2) #if you set really narrow pdfs you  need to round to more digits to get an accurate mode
  # uniqv <- unique(dist) #groups same numbers
  # mode <- uniqv[which.max(tabulate(match(dist, uniqv)))] #which number occurs most often
  # print(mode)
  # cutoff <- 0 #allows you to cutoff for plotting purposes (to "zoom in" to a specific area) 
  # hist(subset(distribution, distribution > cutoff), breaks = seq(cutoff, 1, 0.005), main = paste("Mode =", mode, ", n = 1,000,000, Alpha & Beta =", round(alpha, digits = 2), "&", round(beta, digits = 2)), xlab = "")
  # #uncomment line below if you want to set mean, and comment line above
  # #hist(subset(distribution, distribution > cutoff), breaks = seq(cutoff,1,0.005), main = paste("Mean =", mean(distribution), ", n = 1,000,000, Alpha & Beta =", round(alpha, digits = 2), "&", round(beta, digits = 2)), xlab = "")
  # abline(v = c(mean(distribution), median(distribution), mode ), col = c("red", "purple", "blue"), lwd = 2)   #plot vertical lines
  # 
  
  # # Log Normal Distribution
  # # Q1:  We want to take the numbers we know and determine what IRR is required given what target we have to achieve
  # # Q2:  An alternative question is:  What A and B do we need to achieve a specified IRR for a specified target
  # A <- rlnorm(n_samples, meanlog=log(target*3), sdlog=0.5)
  # B <- rlnorm(n_samples, meanlog=log(target*0.3), sdlog=0.6)
  # PNC <- rnorm(100000, mean = input$PF_NC_mean, sd = input$PF_NC_sd)
  # PC <- rnorm(100000, mean = input$PF_C_mean, sd = input$PF_C_sd)
  # PoF <- rnorm(100000, mean = input$Targ_mean, sd = input$Targ_sd)
  # 
  # IRR <- round((PNC - PoF)/(PNC - PC),digits = 9)
  # 
  # df <- data.frame("PNC" = PNC, "PC" = PC, "PoF" = PoF, "IRR" = IRR)
  # 
  # df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
  # 
  # df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
  # 
  # return(df)
  # 
  # aleatoric_data <- reactive({
  #   PNC <- rnorm(100000, mean = input$PF_NC_mean, sd = input$PF_NC_sd)
  #   PC <- rnorm(100000, mean = input$PF_C_mean, sd = input$PF_C_sd)
  #   PoF <- rnorm(100000, mean = input$Targ_mean, sd = input$Targ_sd)
  #   
  #   IRR <- round((PNC - PoF)/(PNC - PC),digits = 9)
  #   
  #   df <- data.frame("PNC" = PNC, "PC" = PC, "PoF" = PoF, "IRR" = IRR)
  #   
  #   df <- df[df$PNC >= df$PoF & df$PC <= df$PoF & df$PC < df$PNC,]
  #   
  #   df <- df[df$PNC >= 0 & df$PNC <= 1 & df$PC >= 0 & df$PC <= 1,]
  #   
  #   return(df)
  #   
  # })
  # 
  
  #############################################################################################
  
  ## For Contour Plot Panel - Main ###########################################################
  
  # For general contour plot ##################################################################
  
  output$contPlot <- renderPlotly({
    
    # Safe input handling
    ptarget <- 0.1  # Default
    tryCatch({
      if(exists("input") && !is.null(input$rbsTarget)) {
        if(input$rbsTarget == "select" && !is.null(input$rbsSelect)){
          ptarget <- as.numeric(input$rbsSelect)
        } else if(input$rbsTarget == "manual" && !is.null(input$Ptarget)){
          ptarget <- input$Ptarget
        }
      }
    }, error = function(e) {
      print("Using default ptarget due to input error")
    })
    
    # print("=== DYNAMIC PROGRESSIVE RESOLUTION ===")
    # print(paste("Target:", ptarget))
    
    # CALCULATE PRECISION PARAMETERS DYNAMICALLY
    # Determine number of decimal places needed
    if (ptarget >= 0.1) {
      precision_digits <- 1
    } else {
      # Count decimal places: 0.01 -> 2, 0.001 -> 3, 0.0001 -> 4, etc.
      precision_digits <- -floor(log10(ptarget))
    }
    
    # print(paste("Precision digits:", precision_digits))
    
    # DYNAMIC THRESHOLD CALCULATION
    # Create progressive thresholds based on target precision
    thresholds <- list()
    step_sizes <- list()
    colors <- c("black", "red", "blue", "green", "purple", "orange", "brown", "pink", "gray")
    
    # Always start with base: 0.5 to 0.95 (adjusted to make room for new layer)
    thresholds[[1]] <- c(0.5, 0.95)
    step_sizes[[1]] <- 0.05
    
    # NEW: Medium layer from 0.95 to 0.99 by 0.01 increments
    thresholds[[2]] <- c(0.95, 0.99)
    step_sizes[[2]] <- 0.01
    
    # print("Adding medium precision layer: 0.95 to 0.99 by 0.01 (black contours)")
    
    # Create progressive layers based on precision needs (starting from position 3)
    current_start <- 0.99
    for (i in 1:min(precision_digits, 7)) {  # Reduced to 7 since we added one layer
      
      # Calculate the end threshold for this layer
      if (i == 1) {
        current_end <- 0.999
        step_size <- 0.001  # 3 decimal places
      } else if (i == 2) {
        current_end <- 0.9999  
        step_size <- 0.0001  # 4 decimal places
      } else if (i == 3) {
        current_end <- 0.99999
        step_size <- 0.00001  # 5 decimal places
      } else {
        # For very high precision, calculate dynamically
        nines <- paste(rep("9", i + 2), collapse = "")
        current_end <- as.numeric(paste0("0.", nines))
        step_size <- 10^(-(i + 2))
      }
      
      # Ensure we don't exceed 1.0
      current_end <- min(current_end, 0.999999)
      
      thresholds[[i + 2]] <- c(current_start, current_end)
      step_sizes[[i + 2]] <- step_size
      
      # print(paste("Layer", i + 2, ": from", current_start, "to", current_end, "step", step_size))
      
      current_start <- current_end
      
      # Stop if we're very close to 1.0
      if (current_end >= 0.999999) break
    }
    
    # Add final layer to 1.0 if needed
    if (current_start < 0.999999) {
      final_step <- step_sizes[[length(step_sizes)]] / 10  # Even finer for final layer
      thresholds[[length(thresholds) + 1]] <- c(current_start, 1.0)
      step_sizes[[length(step_sizes) + 1]] <- final_step
      #print(paste("Final layer: from", current_start, "to 1.0 step", final_step))
    }
    
    # High-resolution grid (adaptive based on target)
    pNC_coarse <- seq(from = ptarget, to = 0.9, by = 0.02)
    pNC_medium <- seq(from = 0.9, to = 0.98, by = 0.005)  
    pNC_fine <- seq(from = 0.98, to = 1.0, by = 0.002)
    pNC_all <- sort(unique(c(pNC_coarse, pNC_medium, pNC_fine)))
    
    # Adaptive pC resolution
    pC_steps <- ptarget / (100 * precision_digits)  # Finer resolution for higher precision targets
    pC_all <- seq(from = 0, to = ptarget, by = pC_steps)
    
    #print(paste("Grid: pNC =", length(pNC_all), "points, pC =", length(pC_all), "points"))
    
    # Data creation
    grid_data <- expand.grid(pNC = pNC_all, pC = pC_all)
    grid_data$IRR <- (grid_data$pNC - ptarget) / (grid_data$pNC - grid_data$pC)
    grid_data$IRR[grid_data$IRR < 0.5] <- 0.5
    grid_data$IRR[is.na(grid_data$IRR)] <- 0.5
    
    z_matrix <- matrix(grid_data$IRR, 
                       nrow = length(pC_all), 
                       ncol = length(pNC_all),
                       byrow = TRUE)
    
    #print(paste("Matrix:", nrow(z_matrix), "x", ncol(z_matrix)))
    
    # CREATE BASE FILLED CONTOURS
    fig <- plot_ly(
      x = pNC_all, y = pC_all, z = z_matrix,
      type = "contour", colorscale = "Cividis",
      autocontour = FALSE,
      contours = list(
        start = thresholds[[1]][1],  # 0.5
        end = thresholds[[1]][2],    # 0.99
        size = step_sizes[[1]],      # 0.05
        coloring = "fill", 
        showlabels = TRUE,
        labelfont = list(color = "white", size = 12)
      )
    )
    
    # ADD PROGRESSIVE PRECISION LAYERS DYNAMICALLY
    for (i in 2:length(thresholds)) {
      layer_start <- thresholds[[i]][1]
      layer_end <- thresholds[[i]][2]
      layer_step <- step_sizes[[i]]
      layer_color <- colors[min(i-1, length(colors))]
      
      #print(paste("Adding layer", i, ":", layer_color, "from", layer_start, "to", layer_end, "step", layer_step))
      
      # Skip if step size is too small (would create too many contours)
      if (layer_step < 0.000001) {
        print(paste("Skipping layer", i, "- step size too small"))
        next
      }
      
      fig <- fig %>% add_trace(
        type = "contour", z = z_matrix, x = pNC_all, y = pC_all,
        autocontour = FALSE, showscale = FALSE,
        contours = list(
          start = layer_start,
          end = layer_end,
          size = layer_step,
          coloring = "lines", 
          line = list(color = layer_color, width = max(3 - i * 0.3, 0.5)),  # Progressively thinner lines
          showlabels = TRUE, 
          labelfont = list(color = layer_color, size = max(12 - i, 8))  # Progressively smaller labels
        )
      )
    }
    
    # Enhanced layout
    precision_description <- paste("Progressive Resolution -", length(thresholds), "layers,", precision_digits, "decimal precision")
    
    fig <- fig %>% layout(
      title = list(
        text = paste0("IRR Contour Plot with Dynamic Progressive Resolution<br><sub>Target: ", ptarget, " | ", precision_description, "</sub>"),
        font = list(size = 16)
      ),
      xaxis = list(
        title = "Probability of Failure Given Nonconformance",
        exponentformat = "E",
        range = c(ptarget, 1.0),
        autorange = FALSE
      ),
      yaxis = list(
        title = "Probability of Failure Given Conformance",
        exponentformat = "E", 
        range = c(0, ptarget),
        autorange = FALSE
      ),
      margin = list(t = 100, b = 60, l = 80, r = 100),
      hovermode = 'x+y'
    )
    
    fig <- fig %>% colorbar(title = "IRR Values")
    fig <- event_register(fig, 'plotly_hover')
    
    # Add cursor tracking
    fig <- fig %>%
      htmlwidgets::onRender("
    function(el, x) {
      setTimeout(function() {
        el.addEventListener('mousemove', function(evt) {
          var layout = el._fullLayout;
          if (!layout || !layout.xaxis || !layout.yaxis) return;
          
          var rect = el.getBoundingClientRect();
          var mouseX = evt.clientX - rect.left;
          var mouseY = evt.clientY - rect.top;
          
          var margin = layout.margin;
          var plotLeft = margin.l;
          var plotTop = margin.t;
          var totalPlotWidth = layout.width - margin.l - margin.r;
          var plotHeight = layout.height - margin.t - margin.b;
          
          if (mouseX >= plotLeft && mouseX <= plotLeft + totalPlotWidth &&
              mouseY >= plotTop && mouseY <= plotTop + plotHeight) {
            
            var relativeX = (mouseX - plotLeft) / totalPlotWidth;
            var relativeY = (mouseY - plotTop) / plotHeight;
            
            var dataX = layout.xaxis.range[0] + relativeX * (layout.xaxis.range[1] - layout.xaxis.range[0]);
            var dataY = layout.yaxis.range[1] - relativeY * (layout.yaxis.range[1] - layout.yaxis.range[0]);
            
            Shiny.setInputValue('responsive_coords', {
              x: dataX,
              y: dataY,
              timestamp: Date.now()
            });
          }
        });
      }, 2000);
    }
    ")
    
    fig <- fig %>% config(
      displayModeBar = TRUE,  # Show the toolbar
      modeBarButtonsToRemove = c(
        'select2d',
        'lasso2d',
        'autoScale2d',
        'hoverClosestCartesian',
        'hoverCompareCartesian',
        'toggleSpikelines'
      ),
      # Alternative: specify only buttons to keep
      # modeBarButtonsToAdd = c(),  # Add custom buttons if needed
      # modeBarButtons = list(list('zoom2d', 'pan2d', 'zoomIn2d', 'zoomOut2d', 'resetScale2d')),
      displaylogo = FALSE,  # Remove plotly logo
      toImageButtonOptions = list(
        format = 'png',
        filename = 'IRR_plot',
        height = 600,
        width = 800,
        scale = 1
      )
    )
    
    return(fig)
  })  
  
  # For general plot coordinate values Table ####################################################
  
  output$current_values <- renderTable({
    #Use plotly hover Z if available, otherwise calculate manually
    
    if (!is.null(input$responsive_coords)) {
      coords <- input$responsive_coords
      
      x_val <- coords$x  # pNC
      y_val <- coords$y  # pC
      
      # Get current ptarget
      if(input$rbsTarget == "select"){
        ptarget <- as.numeric(input$rbsSelect)
      } else if(input$rbsTarget == "manual"){
        ptarget <- input$Ptarget
      }
      
      # Use the exact same formula as in your IRRFrame creation
      z_val <- (x_val - ptarget) / (x_val - y_val)
      
      # Apply the same filtering as in your plot
      if (z_val < input$minScale) {
        z_val <- input$minScale
      }
    } else {
      return(NULL)
    }
    
    data.frame(
      Parameter = c("Prob. Failure | Non-Conformance", "Prob. Failure | Conformance", "IRR Value"),
      Value = c(
        sprintf("%.6f", x_val),
        sprintf("%.6f", y_val),
        sprintf("%.6f", z_val)
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # for Threshold Contour Plot ######################################################################
  
  output$contPlot2 <- renderPlotly({
    
    # Safe requirement check
    if (is.null(input$cbEval) || !input$cbEval) {
      return(NULL)
    }
    
    # Safe input handling
    ptarget <- 0.1  # Default
    tryCatch({
      if(exists("input") && !is.null(input$rbsTarget)) {
        if(input$rbsTarget == "select" && !is.null(input$rbsSelect)){
          ptarget <- as.numeric(input$rbsSelect)
        } else if(input$rbsTarget == "manual" && !is.null(input$Ptarget)){
          ptarget <- input$Ptarget
        }
      }
    }, error = function(e) {
      print("Using default ptarget due to input error")
    })
    
    # Get selIRR threshold
    selIRR <- 0.99   # Default threshold
    tryCatch({
      if(exists("input") && !is.null(input$selIRR)) {
        selIRR <- as.numeric(input$selIRR)
      }
    }, error = function(e) {
      print("Using default selIRR due to input error")
    })
    
    # Grid creation
    if (ptarget >= 0.1) {
      precision_digits <- 1
    } else {
      precision_digits <- -floor(log10(ptarget))
    }
    
    pNC_coarse <- seq(from = ptarget, to = 0.9, by = 0.02)
    pNC_medium <- seq(from = 0.9, to = 0.98, by = 0.005)  
    pNC_fine <- seq(from = 0.98, to = 1.0, by = 0.002)
    pNC_all <- sort(unique(c(pNC_coarse, pNC_medium, pNC_fine)))
    
    pC_steps <- ptarget / (100 * precision_digits)
    pC_all <- seq(from = 0, to = ptarget, by = pC_steps)
    
    # Data creation
    grid_data <- expand.grid(pNC = pNC_all, pC = pC_all)
    grid_data$IRR <- (grid_data$pNC - ptarget) / (grid_data$pNC - grid_data$pC)
    grid_data$IRR[grid_data$IRR < 0.5] <- 0.5
    grid_data$IRR[is.na(grid_data$IRR)] <- 0.5
    
    z_matrix <- matrix(grid_data$IRR, 
                       nrow = length(pC_all), 
                       ncol = length(pNC_all),
                       byrow = TRUE)
    
    # Create ultra-high resolution grid for smooth edges
    pNC_fine <- seq(from = min(pNC_all), to = max(pNC_all), length.out = length(pNC_all) * 3)
    pC_fine <- seq(from = min(pC_all), to = max(pC_all), length.out = length(pC_all) * 3)
    
    # Recreate data at ultra-high resolution
    grid_data_fine <- expand.grid(pNC = pNC_fine, pC = pC_fine)
    grid_data_fine$IRR <- (grid_data_fine$pNC - ptarget) / (grid_data_fine$pNC - grid_data_fine$pC)
    grid_data_fine$IRR[grid_data_fine$IRR < 0.5] <- 0.5
    grid_data_fine$IRR[is.na(grid_data_fine$IRR)] <- 0.5
    
    z_matrix_fine <- matrix(grid_data_fine$IRR, 
                            nrow = length(pC_fine), 
                            ncol = length(pNC_fine),
                            byrow = TRUE)
    
    # Binary color assignment
    z_color_fine <- ifelse(z_matrix_fine <= selIRR, 0, 1)
    
    # Create heatmap background
    fig <- plot_ly(
      x = pNC_fine, 
      y = pC_fine, 
      z = z_color_fine,
      type = "heatmap",
      colorscale = list(
        list(0, "green"),
        list(1, "red")
      ),
      showscale = FALSE,
      zmin = 0,
      zmax = 1
    )
    
    # Calculate label formatting
    if (grepl("\\.", as.character(selIRR))) {
      selIRR_str <- gsub("0+$", "", as.character(selIRR))
      selIRR_str <- gsub("\\.$", "", selIRR_str)
      decimal_places <- nchar(sub(".*\\.", "", selIRR_str))
    } else {
      decimal_places <- 0
    }
    decimal_places <- max(decimal_places, 2)
    format_string <- paste0(".", decimal_places, "f")
    
    # Add threshold contour line
    fig <- fig %>% add_trace(
      type = "contour", 
      z = z_matrix_fine,
      x = pNC_fine, 
      y = pC_fine,
      autocontour = FALSE, 
      showscale = FALSE,
      contours = list(
        coloring = 'lines',
        line = list(color = 'black', width = 8),
        showlabels = TRUE,
        start = selIRR,
        end = selIRR,
        labelfont = list(color = "black", size = 18, family = "Arial Black"),
        labelformat = format_string,
        labelprefix = "IRR = "
      )
    )
    
    # Layout
    fig <- fig %>% layout(
      title = list(
        text = paste0("IRR Threshold Assessment<br><sub>Target: ", ptarget, " | Threshold: ", selIRR, " | Green: ≤ Threshold, Red: > Threshold</sub>"),
        font = list(size = 16)
      ),
      xaxis = list(
        title = "Probability of Failure Given Nonconformance",
        exponentformat = "E",
        range = c(ptarget, 1.0),
        autorange = FALSE
      ),
      yaxis = list(
        title = "Probability of Failure Given Conformance",
        exponentformat = "E", 
        range = c(0, ptarget),
        autorange = FALSE
      ),
      margin = list(t = 100, b = 60, l = 80, r = 100),
      hovermode = 'x+y'
    )
    
    fig <- fig %>% config(
      displayModeBar = TRUE,  # Show the toolbar
      modeBarButtonsToRemove = c(
        'select2d',
        'lasso2d',
        'autoScale2d',
        'hoverClosestCartesian',
        'hoverCompareCartesian',
        'toggleSpikelines'
      ),
      # Alternative: specify only buttons to keep
      # modeBarButtonsToAdd = c(),  # Add custom buttons if needed
      # modeBarButtons = list(list('zoom2d', 'pan2d', 'zoomIn2d', 'zoomOut2d', 'resetScale2d')),
      displaylogo = FALSE,  # Remove plotly logo
      toImageButtonOptions = list(
        format = 'png',
        filename = 'IRR_plot',
        height = 600,
        width = 800,
        scale = 1
      )
    )
    
    return(fig)
  })
  
  output$checker <- renderTable({
    glimpse(aleatoric_data_s15()) # something that relies on the reactive, same thing here for simplicty
  })
  
  
  ########################################################################################################
  
  ### Closing out Server 
  
}

#########################################################################################################

