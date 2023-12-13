
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
  })

  num_var_1 <- eventReactive(input$run_button, input$num_var_1)
  num_var_2 <- eventReactive(input$run_button, input$num_var_2)
  num_var_3 <- eventReactive(input$run_button, input$num_var_3)
  fact_var <- eventReactive(input$run_button, input$fact_var)

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

    # Kaplan-Meier estimator of event free time
    KM <- survival::survfit(survival::Surv(dt,
                                           event) ~ 1,
                            type = "kaplan-meier",
                            conf.type = "log-log",
                            data = data)

    # Figure 1. Kaplan–Meier estimate of survivor function for overall survival, X,
    # with 95% confidence intervals and numbers at risk
    # (compare Figure 1 in Betensky (2015))
    plot1 <- survminer::ggsurvplot(KM,
                                   data = data,
                                   risk.table = TRUE,
                                   conf.int = TRUE,
                                   censor = TRUE,
                                   title = "")


    ################################################################################
    # stability interval = lower and upper limit for KM (Betensky, 2015)           #
    ################################################################################

    # calculation of upper limit
    # by "setting all censored observations to a value larger than the maximum event
    # time (and retaining their status as censored)"
    #-------------------------------------------------------------------------------
    # maximum event time
    # df <- subset(data, event == 1)
    # maximum_event_time <- ceiling(max(df$dt))
    maximum_event_time <- ceiling(max(data$dt))
    # set all time of censored observations to the maximum event time
    data$timeupper <- data$dt
    data$timeupper[data$event == 0] <- maximum_event_time

    # calculation of lower limit
    # by "coding all censored observations as events at the observed event times
    # immediately following their censoring times"
    #-------------------------------------------------------------------------------
    data$time <- data$dt
    dfC <- subset(data, event == 0)
    dfE <- subset(data, event == 1)

    for(i in 1:length(dfC$time)){

      # difference of censoring time of censor event i (Ci) and
      # observed event times following this event (E): m = E - Ci
      m <- ((dfE$time - dfC$time[i])[(dfE$time - dfC$time[i]) > 0])

      if(length(m) > 0){
        # replace time of censoring of event i with observed event time
        # immediately following this event (Ei): Ci + min(m) = Ci + Ei - Ci = Ei
        dfC$timelower[i] <- dfC$time[i] + min(m)
      }else{
        # if there is no event time following the censoring event i: no replacement?
        dfC$timelower[i] <- dfC$time[i]
      }

    }
    # keep times of events
    dfE$timelower <- as.numeric(dfE$time)
    data <- rbind(dfC, dfE)
    # code all censored observations as events
    data$eventlower <- 1

    DF = data.frame(strata = c(rep("KM estimate", dim(data)[1]),
                               rep("SI lower bound", dim(data)[1]),
                               rep("SI upper bound", dim(data)[1])),
                    time = c(data$time,
                             data$timelower,
                             data$timeupper),
                    event = c(data$event,
                              data$eventlower,
                              data$event))
    DF$strata <- factor(DF$strata, levels = c("SI upper bound", "KM estimate", "SI lower bound"))


    KM <- survfit(Surv(time, event) ~ strata,
                  type = "kaplan-meier",
                  conf.type = "log-log",
                  data = DF)
    # Figure 2. Proposed upper and lower limits for Kaplan–Meier.
    plot2 <- ggsurvplot(KM,
               data = DF,
               risk.table = TRUE,
             #  conf.int = TRUE,
               censor = TRUE,
               title = "",
               legend = "none",
               palette = c("gray50", "red", "gray51"))

    # metrics derived from the upper and lower limits
    #-------------------------------------------------------------------------------
    # quantile summaries of limits:
    frame <- as.data.frame(quantile(KM, probs = c(0.25, 0.5, 0.75), conf.int = TRUE))

    names(frame) <- c("0.25quantile",
                      "median",
                      "0.75quantile",
                      "lower CI 0.25quantile",
                      "lower CI median",
                      "lower CI 0.75quantile",
                      "upper CI 0.25quantile",
                      "upper CI median",
                      "upper CI 0.75quantile")
    row.names(frame) <- c("SI upper bound", "KM estimate", "SI lower bound")

    frame_pretty <- cbind(c("SI upper bound", "KM estimate", "SI lower bound"),
                          t(t(paste0(frame$`0.25quantile`, " (",
                                     frame$`lower CI 0.25quantile`, ",",
                                     frame$`upper CI 0.25quantile`, ")"))),
                          t(t(paste0(frame$median, " (",
                                     frame$`lower CI median`, ",",
                                     frame$`upper CI median`, ")"))),
                          t(t(paste0(frame$`0.75quantile`, " (",
                                     frame$`lower CI 0.75quantile`, ",",
                                     frame$`upper CI 0.75quantile`, ")"))))

    frame_pretty <- as.data.frame(frame_pretty)
    names(frame_pretty)<- c("","0.25", "0.5", "0.75")

    # difference curve between the upper and lower limits and the area under this curve,
    # normalized by the maximum event time, to range between 0 (complete stability) and 1 (complete instability)
    DF_norm <- surv_summary(KM, data = DF)
    DF_norm <- subset(DF_norm, time <= maximum_event_time)
    keep <- as.numeric(names(which(table(DF_norm$time) == 3)))
    DF_norm <- subset(DF_norm, time %in% keep)
    time <- subset(DF_norm, strata == "SI upper bound")$time
    diff_upper_lower <- subset(DF_norm, strata == "SI upper bound")$surv - subset(DF_norm, strata == "SI lower bound")$surv
    f <- approxfun(time, diff_upper_lower, method = "constant")
    auc <- integrate(f,
                     lower = min(time),
                     upper = max(time),
                     subdivisions = 10000L)
    norm_auc <- auc$value/as.numeric(maximum_event_time)

    plot(time, f(time), type = "l")
    lines(time, diff_upper_lower, type = "p", col = "red")

    # partial difference curves to indicate directional instability:
    # the difference between the upper limit and the Kaplan–Meier estimate and
    diff_upper_KM <- subset(DF_norm, strata == "SI upper bound")$surv - subset(DF_norm, strata == "KM estimate")$surv

    # the difference between the Kaplan–Meier estimate and the lower limit.
    diff_KM_lower <- subset(DF_norm, strata == "KM estimate")$surv - subset(DF_norm, strata == "SI lower bound")$surv

    # Figure 4. Difference curve between upper and lower limits of Kaplan–Meier
    # and partial difference curves between Kaplan–Meier and upper and lower limits.
    DF4 <- data.frame(time = c(time, time, time),
                      strata = c(rep("SI upper limit minus SI lower limit", length(time)),
                                 rep("SI upper limit minus KM estmate", length(time)),
                                 rep("KM estimate minus SI lower limit", length(time))),
                      Probability = c(diff_upper_lower,
                                      diff_upper_KM,
                                      diff_KM_lower))
    DF4$strata <- factor(DF4$strata, levels = c("SI upper limit minus SI lower limit",
                                                "SI upper limit minus KM estmate",
                                                "KM estimate minus SI lower limit"))

    plot4 <- ggplot(data = DF4,
                    aes(x = time, y = Probability, group = strata)) +
     # ggtitle("Difference curve between upper and lower limits of Kaplan–Meier and partial difference curves between Kaplan–Meier and upper and lower limits") +
      geom_line(aes(linetype = strata)) +
      scale_linetype_manual(name="", values = c("solid", "dashed", "dotted"),
                            labels=c(paste0("SI upper limit - SI lower limit;\n normalized auc = ",
                                            round(norm_auc, 2 )),
                                     "SI upper limit - KM estmate",
                                     "KM estimate - SI lower limit")) +
      theme_survminer()

    ################################################################################
    # measures of follow-up as a measure of the stability of the KM estimator      #
    ################################################################################
    dfC <- subset(data, event == 0)
    FU = data.frame(strata = c(rep("C", dim(data)[1]), # time to censoring
                               rep("T = min(X, C)", dim(data)[1]), # observation time
                               rep("C|C<X", dim(dfC)[1])), # observation time among those who are censored
                    time = c(data$time,
                             data$time,
                             dfC$time),
                    event = c(1 - data$event,
                              rep(1, dim(data)[1]),
                              rep(1, dim(dfC)[1])))

    KM_FU <- survfit(Surv(time, event) ~ strata,
                     type = "kaplan-meier",
                     conf.type = "log-log",
                     data = FU)

    # derived summary measures of measures of follow-up as measure of the stability of the KM estimates
    frame1 <- as.data.frame(quantile(KM_FU, probs = c(0.25, 0.5, 0.75), conf.int = TRUE))

    names(frame1) <- c("0.25quantile",
                      "median",
                      "0.75quantile",
                      "lower CI 0.25quantile",
                      "lower CI median",
                      "lower CI 0.75quantile",
                      "upper CI 0.25quantile",
                      "upper CI median",
                      "upper CI 0.75quantile")
    row.names(frame1) <- c("C", "C|C<X", "T = min(X, C)")

    frame1_pretty <- cbind(c("C", "C|C<X", "T = min(X, C)"),
                           t(t(paste0(frame1$`0.25quantile`, " (",
                                      frame1$`lower CI 0.25quantile`, ",",
                                      frame1$`upper CI 0.25quantile`, ")"))),
                           t(t(paste0(frame1$median, " (",
                                      frame1$`lower CI median`, ",",
                                      frame1$`upper CI median`, ")"))),
                           t(t(paste0(frame1$`0.75quantile`, " (",
                                      frame1$`lower CI 0.75quantile`, ",",
                                      frame1$`upper CI 0.75quantile`, ")"))))

    frame1_pretty <- as.data.frame(frame1_pretty)
    names(frame1_pretty)<- c("","0.25", "0.5", "0.75")

    # Figure 3
    plot3 <- ggsurvplot(KM_FU,
               data = FU,
               risk.table = TRUE,
               conf.int = TRUE,
               censor = TRUE)

    list(plot1, plot2, plot3, plot4, frame, frame1, frame_pretty, frame1_pretty)

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


  output$num_var_1_title <- renderText("Table 1a: Quantile summaries of KM estimate and proposed SI upper and lower bounds with associated 95% confidence intervals (lower CI, upper CI)")
  output$num_var_2_title <- renderText("Table 2a: Quantile summaries of C, C|C<X and T=min(X, c) with associated 95% confidence intervals (lower CI, upper CI)")

  output$num_var_6_title <- renderText("Tables for secondary analysis")

  output$num_var_3_title <- renderText("Table 1b: Quantile summaries of KM estimate and proposed SI upper and lower bounds with associated 95% confidence intervals (lower CI, upper CI)")
  output$num_var_4_title <- renderText("Table 2b: Quantile summaries of C, C|C<X and T=min(X, c) with associated 95% confidence intervals (lower CI, upper CI)")


  output$titlefig1 <- renderText("Figure 1: Kaplan–Meier estimate of survivor function for overall survival, with 95% confidence intervals and numbers at risk.")
  output$titlefig2 <- renderText("Figure 2: stability interval upper and lower limits for Kaplan–Meier estimate as proposed by Betensky (2015).")
  output$titlefig3 <- renderText("Figure 3: Kaplan–Meier estimates of time to censoring, C, observation time, T, and time to censoring among those who are censored, C|C<X.")
  output$titlefig4 <- renderText("Figure 4: Difference curve between upper and lower limits of Kaplan–Meier and partial difference curves between Kaplan–Meier and upper and lower limits.")

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

