library(shinydashboard)
library(shinyWidgets)
library(Project2AndrewMitchell)

#' Shiny Server Function
#'
#' @param input the input of the function
#' @param output the output of the function
#'
#' @return a shiny server
#' @export
#'
#' @examples server()
server<-function(input,output){

  output$plot1<-renderPlot({

    plotter(plotType=input$plotType,
            mile=input$mile,length=input$length,weight=input$weight,ddt=input$ddt,
            FCM=input$FCM,LCM=input$LCM,SCM=input$SCM,TRM=input$TRM,
            catfish=input$catfish,buffalo=input$buffalo,bass=input$bass,
            pcaType=input$pcaType,col=input$col)

  })
}
