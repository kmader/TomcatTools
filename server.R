library(shiny)
library(ggplot2)
library(png)
library(ggthemes)

shinyServer(function(input, output) {
  get.slices<-reactive({
    c.files
  })
  get.name<-reactive({
    if(is.null(input$cur_file)) ""
    else input$cur_file
  })
  output$img_selector<-renderUI({
    if (length(get.slices())>1) {
      wellPanel(h4("Slice to Read"),
                selectInput('cur_file', 'Image Name', get.slices())) 
    } else h3("No Image Loaded")
  })
  get.min<-reactive({
    if(is.null(input$min_val)) 0.00
    else {
      input$min_val
    }
  })
  
  get.max<-reactive({
    if(is.null(input$max_val)) 0.008
    else {
      input$max_val
    }
  })
  get.slice.vals<-reactive({
    read.dmp.vals(get.name())
  })
  get.scaled.vals<-reactive({
    cin<-get.slice.vals()
    gvals<-cin$vals
    gvals<-(gvals-get.min())/(get.max()-get.min())
    gvals[which(gvals<0)]<-0
    gvals[which(gvals>1)]<-1
    cin$vals<-gvals
    cin
  })
  get.slice<-reactive({
    rda<-dmp.val.to.img(get.scaled.vals())
    
  })
  
  output$minmaxselector<-renderUI({
    gvals<-get.slice.vals()$vals
    if (length(gvals)>1) {
      wellPanel(sliderInput('min_val', 'Minimum Value',
                  min=min(gvals), max=max(gvals),
                  value=min(gvals)),
                sliderInput('max_val', 'Maximum Value',
                            min=min(gvals), max=max(gvals),
                            value=max(gvals)),
                h4(paste("Cur Position",input$previewClick$x,", y",input$previewClick$x))) 
    } else h3("No Image Loaded")
  })
  output$preview<-renderImage({
    outfile <- tempfile(fileext='.png')
    outfile<-"out.png"
    writePNG(get.slice(),outfile)
    list(src=outfile,alt="Current Slice",height=400)
  },deleteFile=FALSE)
  
  output$hist<-renderPlot({
    p.plot<-ggplot(data.frame(vals=get.slice.vals()$vals),aes(x=vals,y=..scaled..))+geom_density()
    xlims<-data.frame(x=c(get.min(),get.max()))
    p.plot<-p.plot+geom_segment(data=xlims,aes(x=x,xend=x,y=0,yend=1,color="Limits"))
    p.plot<-p.plot+theme_wsj(20)+labs(x="Absorption Value (au)",y="Frequency (au)",color=NA,title="Histogram")
    print(p.plot)
  })
  
})