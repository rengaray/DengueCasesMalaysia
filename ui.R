library(shiny)
# Present Year and Plotting Options for Users
data_years <- c("ALL","2011","2012", "2013","2014","2015")
data_graf <- c("By Year","By State", "Trend By Week")

fluidPage(
  pageWithSidebar(
    headerPanel(h2("Dengue Cases Trend in Malaysia by State (2011-2015)")
                ),

    sidebarPanel(
      helpText("Please select ALL or the Respective Year",
               "and view \"Summary of Cases By State\" tab"),
      selectInput("datayear", label = "Please Select Year:", choices = data_years),
      helpText("Please select Graphical representation needed",
               "and view \"Graphical Representation\" tab"),
      selectInput("graf1", label = "Select Plot Required:", choices = data_graf),
      br(),
      helpText("If selecting \"Trend By Week\", please choose the Week:"),
      sliderInput("dataweekEnd","Week",1,52,1,1),
      br(),
      a(href = "https://github.com/rengaray/DengueCasesMalaysia.git", "The Shiny Apps server.R and ui.R Source code")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary of Cases By State", tableOutput("data_table")),
        tabPanel("Graphical Representation", plotOutput("data_plot")))
      )
  )
)