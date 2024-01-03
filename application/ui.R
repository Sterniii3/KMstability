ui <- navbarPage(
  title = "Stability of Kaplan-Meier estimates",
  #  theme = shinytheme('united'),
  tabPanel(
    title = "Analysis",
 #   titlePanel("Analysis"),
    sidebarLayout(
      sidebarPanel(
        title = "Inputs",
        fileInput("csv_input", "1. Select csv file for import", accept = ".csv"),

        selectInput(
          inputId = "option",
          label = "2. choose input option",
          choices = c("start and stop date",
                      "time interval",
                      ""),
          selected = ""),

        h5(strong('3. specify names of variables')),

        conditionalPanel(
          condition = "input.option == 'start and stop date'",

          selectInput("num_var_1", "start date", choices = c("start_date")),
          selectInput("num_var_2", "stop date", choices = c("stop_date"))
          ),

        conditionalPanel(
          condition = "input.option == 'time interval'",
          selectInput("num_var_3", "time interval", choices = c("time"))
        ),


        selectInput("fact_var", "event", choices = c("event")),

        selectInput(
          inputId = "option1",
          label = "4. choose number of groups",
          choices = c("1",
                      "2",
                      ""),
          selected = ""),

        conditionalPanel(
          condition = "input.option1 == 2",

          selectInput("fact_var1", "group", choices = c("group")),
        ),

        radioButtons("Download Option", "5. choose the format", list("png","jpeg","pdf")),
        br(),
        actionButton("run_button", "Go"),
        h6(strong('Details on the implemented calculation algorithms can be found in Erdmann (2022), while an interpretation help for the results can be found in Betensky (2015)'))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "plots",
            plotOutput("plot_1"),
            fluidRow(strong(textOutput("titlefig1"))
            ),
            fluidRow(textOutput("titlefig1a")
            ),
            downloadButton("save1", "save Figure 1"),
            br(),
            br(),
            plotOutput("plot_2"),
            fluidRow(strong(textOutput("titlefig2"))
            ),
            fluidRow(textOutput("titlefig2a")
            ),
            downloadButton("save2", "save Figure 2"),
            br(),
            br(),
            plotOutput("plot_3"),
            fluidRow(strong(textOutput("titlefig3"))
            ),
            fluidRow(textOutput("titlefig3a")
            ),
            downloadButton("save3", "save Figure 3"),
            br(),
            br(),
            plotOutput("plot_4"),
            fluidRow(strong(textOutput("titlefig4"))
            ),
            fluidRow(textOutput("titlefig4a")
            ),
            downloadButton("save4", "save Figure 4")
          ),
          tabPanel(
            title = "derived metrics",
            br(),
            fluidRow(h3(textOutput("num_var_5_title"))
            ),
            br(),
            fluidRow(strong(textOutput("num_var_1_title"))
            ),
            fluidRow(
              column(width = 10, tableOutput("num_var_1_summary_table")),
            ),
            downloadButton("save5", "save Table 1a"),
            br(),
            br(),
            fluidRow(strong(textOutput("num_var_2_title"))
            ),
            fluidRow(
              column(width = 10, tableOutput("num_var_2_summary_table")),
            ),
            downloadButton("save6", "save Table 2a"),
            br(),
            br(),
            fluidRow(h3(textOutput("num_var_6_title"))
            ),
            br(),
            fluidRow(strong(textOutput("num_var_3_title"))
            ),
            fluidRow(
              column(width = 10, tableOutput("num_var_3_summary_table")),
            ),
            downloadButton("save7", "save Table 1b"),
            br(),
            br(),
            fluidRow(strong(textOutput("num_var_4_title"))
            ),
            fluidRow(
              column(width = 10, tableOutput("num_var_4_summary_table")),
            ),
            downloadButton("save8", "save Table 2b")
          ),

          tabPanel(
            title = "How to use the app",
            includeMarkdown("usage.md")),

          tabPanel(
            title = "Why use the app",
            includeMarkdown("why.md"))
        )
      )
    )
  ),
  tabPanel(
    title = "About",
 #   titlePanel("About"),
 includeMarkdown("help.md"),
 a(href="https://www.klinikum.uni-heidelberg.de/medizinische-biometrie/wir-ueber-uns/wir-ueber-uns",img(src="Logo-IMBI_ENGL_neu.jpg", height=120))


  )
)
