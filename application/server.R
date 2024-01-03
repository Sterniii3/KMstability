
# https://github.com/MatePocs/rshiny_apps/blob/main/data_analyser/app.R
library(shiny)
library(data.table)
library(ggplot2)
library(survminer)
library(survival)


server <- function(input, output){

  options(shiny.maxRequestSize=10*1024^2)

  data_input <- reactive({
    req(input$csv_input)
    fread(input$csv_input$datapath)
  })

  observeEvent(data_input(),{
    choices <- c("Not Selected", names(data_input()))
    updateSelectInput(inputId = "num_var_1", choices = choices)
    updateSelectInput(inputId = "num_var_2", choices = choices)
    updateSelectInput(inputId = "num_var_3", choices = choices)
    updateSelectInput(inputId = "fact_var", choices = choices)
    updateSelectInput(inputId = "fact_var1", choices = choices)
  })

  num_var_1 <- eventReactive(input$run_button, input$num_var_1)
  num_var_2 <- eventReactive(input$run_button, input$num_var_2)
  num_var_3 <- eventReactive(input$run_button, input$num_var_3)
  fact_var <- eventReactive(input$run_button, input$fact_var)
  fact_var1 <- eventReactive(input$run_button, input$fact_var1)

  results <- eventReactive(input$run_button,{

    data <- data_input()

    if(input$option == "start and stop date"){

       names(data)[which(names(data) == num_var_1())] <- "start_date"
       names(data)[which(names(data) == num_var_2())] <- "final_date"
       names(data)[which(names(data) == fact_var())] <- "event"

       data$start_date <- as.Date(data$start_date, tryFormats = c("%d.%m.%Y"))
       data$final_date <- as.Date(data$final_date, tryFormats = c("%d.%m.%Y"))

       data$dt <- difftime(data$final_date, data$start_date)
    }

    if(input$option == "time interval"){

      names(data)[which(names(data) == num_var_3())] <- "dt"
      names(data)[which(names(data) == fact_var())] <- "event"

    }

    if(input$option1 == "2"){

      names(data)[which(names(data) == fact_var1())] <- "group"

      plot1 <- KM_plot_group(data)

      plot2 <- SI_plot_group(data)

      plot3 <- FU_plot_group(data)

      plot4 <- DC_plot_group(data)

      frame <- KM_SI_tab_group(data, pretty = FALSE)
      frame_pretty <- KM_SI_tab_group(data)

      frame1 <- FU_tab_group(data, pretty = FALSE)
      frame1_pretty <- FU_tab_group(data)

      list(plot1, plot2, plot3, plot4, frame, frame1, frame_pretty, frame1_pretty)

    }else{

      plot1 <- KM_plot(data)

      plot2 <- SI_plot(data)

      plot3 <- FU_plot(data)

      plot4 <- DC_plot(data)

      frame <- KM_SI_tab(data, pretty = FALSE)
      frame_pretty <- KM_SI_tab(data)

      frame1 <- FU_tab(data, pretty = FALSE)
      frame1_pretty <- FU_tab(data)

      list(plot1, plot2, plot3, plot4, frame, frame1, frame_pretty, frame1_pretty)
    }





  })

   # output$tab_1 <- renderTable(results()[[1]],
   #                                               rownames = TRUE)

  output$plot_1 <- renderPlot(results()[[1]])

  output$save1 <- downloadHandler(
    #Specify The File Name
    filename = function() {
      paste("Figure1",input$`Download Option`, sep= ".")},
    content = function(file){
      # open the format of file which needs to be downloaded ex: pdf, png etc.
      if (input$`Download Option`== "png"){
        png(file)
      } else if (input$`Download Option`== "pdf"){
        pdf(file, onefile=F)
      } else {
        jpeg(file)
      }
      print(results()[[1]])

      dev.off()
    }
  )

  output$plot_2 <- renderPlot(results()[[2]])

  output$save2 <- downloadHandler(
    #Specify The File Name
    filename = function() {
      paste("Figure2",input$`Download Option`, sep= ".")},
    content = function(file){
      # open the format of file which needs to be downloaded ex: pdf, png etc.
      if (input$`Download Option`== "png"){
        png(file)
      } else if (input$`Download Option`== "pdf"){
        pdf(file, onefile=F)
      } else {
        jpeg(file)
      }
      print(results()[[2]])

      dev.off()
    }
  )

  output$plot_3 <- renderPlot(results()[[3]])

  output$save3 <- downloadHandler(
    #Specify The File Name
    filename = function() {
      paste("Figure3",input$`Download Option`, sep= ".")},
    content = function(file){
      # open the format of file which needs to be downloaded ex: pdf, png etc.
      if (input$`Download Option`== "png"){
        png(file)
      } else if (input$`Download Option`== "pdf"){
        pdf(file, onefile=F)
      } else {
        jpeg(file)
      }
      print(results()[[3]])

      dev.off()
    }
  )

  output$plot_4 <- renderPlot(results()[[4]])

  output$save4 <- downloadHandler(
    #Specify The File Name
    filename = function() {
      paste("Figure4",input$`Download Option`, sep= ".")},
    content = function(file){
      # open the format of file which needs to be downloaded ex: pdf, png etc.
      if (input$`Download Option`== "png"){
        png(file)
      } else if (input$`Download Option`== "pdf"){
        pdf(file, onefile=F)
      } else {
        jpeg(file)
      }
      print(results()[[4]])

      dev.off()
    }
  )

  # 1-d summary tables
  output$num_var_5_title <- renderText("Pretty tables")


  output$num_var_1_title <- renderText("Table 1a: Quantile summaries of Kaplan-Meier estimate (KM) and proposed stability interval (SI) upper and lower bounds with associated 95% confidence intervals (lower CI, upper CI)")
  output$num_var_2_title <- renderText("Table 2a: Quantile summaries of C, C|C<X and T=min(X, c) with associated 95% confidence intervals (lower CI, upper CI)")

  output$num_var_6_title <- renderText("Tables for secondary analysis")

  output$num_var_3_title <- renderText("Table 1a: Quantile summaries of Kaplan-Meier estimate (KM) and proposed stability interval (SI) upper and lower bounds with associated 95% confidence intervals (lower CI, upper CI)")
  output$num_var_4_title <- renderText("Table 2b: Quantile summaries of C, C|C<X and T=min(X, c) with associated 95% confidence intervals (lower CI, upper CI)")



  
  output$titlefig1 <- renderText("Figure 1: Kaplan–Meier estimate with 95% confidence interval and numbers at risk.")
  output$titlefig1a <- renderText("By plotting the confidence interval around the Kapaln-Meier estimate, its precision is visualized. The random 95% confidence interval covers the true parameter in the population with a probability of 95%. Heuristically, this means that if the study is repeated infinitely often (and all assumptions used to compute the confidence intervals were correct), 95% of the confidence intervals would contain the true parameter.")
  output$titlefig2 <- renderText("Figure 2: Upper and lower limits of the stability interval (SIU, SIL) for the Kaplan–Meier estimate (KM) as proposed by Betensky (2015).")
  output$titlefig2a <- renderText("This visualizes the stability of the Kaplan-Meier estimate: the stability interval upper and lower limits are deterministic in the sense that they present a best case (all patients with censoring observations in the study actually survive) and a realistic worst case scenario (all patients actually imminently die after they were censored, whereby unrealistic early events do not occur). Thus, in case all patients were fully observed (no censoring occurs) the associated Kaplan-Meier plot must lay within the stability interval.")
   output$titlefig3a <- renderText("Among the traditional approaches for describing the stability of the Kaplan-Meier estimate, Betensky regarded C | C < X as most directly informative about the stability of the Kaplan-Meier estimate, although it is unstable if there are few event-free observations at the time of analysis. Therefore, the usage of the stability interval is recommended.")
   output$titlefig3 <- renderText("Figure 3: Kaplan–Meier estimates of time to censoring, C, observation time, T, and time to censoring among those who are censored, C|C<X.")
   
   output$titlefig4 <- renderText("Figure 4: Difference curve between upper and lower limits of the stability interval (SIU, SIL) and partial difference curves between Kaplan–Meier estimate (KM) and SIU and SIL, respectively.")
  output$titlefig4a <- renderText("The difference curve shows basically the width of the stability interval as a function of time, while the partial difference curves show the distance of the Kaplan-Meier estimate from the stability interval lower and upper limits, respectively. Therefore, the difference curve indicates the stability of the Kaplan-Meier estimate in terms of the stability interval width as a function of time. The partial difference curves indicate the potential direction of the movement of the Kaplan-Meier estimate for X as a function of time. The normalized area under the difference curve is another measure for stability, where 0 would indicate perfect stability.")
  
  output$num_var_3_summary_table <- renderTable(results()[[5]],
                                                rownames = TRUE)
  output$save5 <- downloadHandler(
    filename = function() {
      'Table1a.csv'
    },
    content = function(file) {
      write.csv(results()[[5]], file)
    }
  )

  output$num_var_4_summary_table <- renderTable(results()[[6]],
                                                rownames = TRUE)
  output$save6 <- downloadHandler(
    filename = function() {
      'Table2a.csv'
    },
    content = function(file) {
      write.csv(results()[[6]], file)
    }
  )


  output$num_var_1_summary_table <- renderTable(results()[[7]],
                                                rownames = TRUE)
  output$save7 <- downloadHandler(
    filename = function() {
      'Table1b.csv'
    },
    content = function(file) {
      write.csv(results()[[7]], file)
    }
  )

  output$num_var_2_summary_table <- renderTable(results()[[8]],
                                                rownames = TRUE)
  output$save8 <- downloadHandler(
    filename = function() {
      'Table2b.csv'
    },
    content = function(file) {
      write.csv(results()[[8]], file)
    }
  )

}

