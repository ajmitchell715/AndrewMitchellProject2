library(shinydashboard)
library(shinyWidgets)

ui <- dashboardPage(
  dashboardHeader(title="Analysis of DDT Data",titleWidth=500),
  dashboardSidebar(disable=TRUE),
  dashboardBody(
    fluidRow(
      box(plotOutput("plot1",height=500)),

      # Box 1: Quantitative Variable Restrictions
      box(
        status="warning",
        solidHeader=TRUE,
        collapsible=TRUE,
        width=3,
        background="red",
        title="Quantitative Variables",
        prettySwitch("mile","Include Mile?",TRUE),
        prettySwitch("length","Include Length?",TRUE),
        prettySwitch("weight","Include Weight?",TRUE),
        prettySwitch("ddt","Include DDT Level?",TRUE)
      ),

      # Box 2: Qualitative Variable Restrictions
      box(
        status="warning",
        solidHeader=TRUE,
        collapsible=TRUE,
        width=3,
        background="blue",
        title="Qualitative Variable Limitations",
        prettySwitch("FCM","Include the FCM River?",TRUE),
        prettySwitch("LCM","Include the LCM River?",TRUE),
        prettySwitch("SCM","Include the SCM River?",TRUE),
        prettySwitch("TRM","Include the TRM River?",TRUE),
        prettySwitch("catfish","Include CCatfish?",TRUE),
        prettySwitch("buffalo","Include SMBuffalo?",TRUE),
        prettySwitch("bass","Include LMBass?",TRUE)
      ),

      # Box 3: Type of Output Plot
      box(
        status="warning",
        solidHeader=TRUE,
        collapsible=TRUE,
        width=3,
        background="orange",
        title="Type of Plot",
        selectInput("plotType","Type of Output",
                    list("scatterplot (1st two variables)","screeplot","biplot",
                         "boxplot (1st variable)"),
                    "screeplot")
      ),

      # Box 4: Plot Settings
      box(
        status="warning",
        solidHeader=TRUE,
        collapsible=TRUE,
        width=3,
        background="purple",
        title="PCA Settings",
        selectInput("pcaType","Type of Matrix for PCA",list("covariance","correlation"),"covariance"),
        selectInput("col","Color Data by?",list("RIVER","SPECIES"),"RIVER")
      )



    )
  )
)
