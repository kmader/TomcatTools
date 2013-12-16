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
               plotOutput('prj_preview',clickId='previewClick',height="600px"),
               checkboxInput("use_color","Color Projection",F),
               checkboxInput("prj_crop","Crop Projection (Zoom)",F),
               conditionalPanel(
                 condition = "input.prj_crop == true",
                 sliderInput('prj_crop_minx', 'Min X', min=1, max=2560,value=1,round=0),
                 sliderInput('prj_crop_maxx', 'Max X', min=1, max=2560,value=2560,round=0),
                 sliderInput('prj_crop_miny', 'Min Y', min=1, max=2160,value=1,round=0),
                 sliderInput('prj_crop_maxy', 'Min Y', min=1, max=2160,value=2160,round=0)
                ),
               uiOutput('minmax_selector'),
               plotOutput('prj_histogram')),
      tabPanel("Log File",tableOutput("log_file")),
      tabPanel("Folder Contents",dataTableOutput("folder_contents"))
      )
    
  )
))