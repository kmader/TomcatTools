library(shiny)
library(ggplot2)



shinyUI(pageWithSidebar(
  
  headerPanel("Beamline Tools"),
  
  sidebarPanel(
    uiOutput("drive_selector"),
    uiOutput("disk_selector"),
    uiOutput("sample_selector")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Projection Viewer",
               checkboxInput("flatfield_correction","Perform flat-field correction",F),
               uiOutput('projection_selector'),
               uiOutput('minmax_selector'),
               plotOutput('prj_preview',clickId='previewClick'),
               plotOutput('prj_histogram')),
      tabPanel("Log File",tableOutput("log_file")),
      tabPanel("Folder Contents",dataTableOutput("folder_contents"))
      )
    
  )
))