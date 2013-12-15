library(shiny)
library(ggplot2)
library(png)
library(ggthemes)
source('recoTools.R')
shinyServer(function(input, output) {
  
  ## path code for selecting the current measurement
  root.dir<-reactive({
    getwd()
  })
  get.drives<-reactive({
    drv.list<-sapply(Sys.glob(papaste(root.dir(),"*/")),
                     pa.getlast)
    as.vector(drv.list)
  })
  get.disks<-reactive({
    as.vector(sapply(Sys.glob(papaste(root.dir(),
                                      get.or.blank(input$drive_selected,"*"),
                                      "*/")),
                              pa.getlast))
  })
  get.samples<-reactive({
    as.vector(sapply(Sys.glob(papaste(root.dir(),
                                      get.or.blank(input$drive_selected,"*"),
                                      get.or.blank(input$disk_selected,"*"),
                                      "*/")),
                              pa.getlast))
  })

  output$drive_selector<-renderUI({
    selectInput("drive_selected","Drive Name",get.drives())
  })
  output$disk_selector<-renderUI({
    selectInput("disk_selected","Disk Name",get.disks())
  })
  output$sample_selector<-renderUI({
    selectInput("sample_selected","Sample Name",get.samples())
  })
  # reading the log information and projections
  get.sample.path<-reactive({
    if(!is.null(input$sample_selected)) papaste(root.dir(),
                                                get.or.blank(input$drive_selected,"*"),
                                                get.or.blank(input$disk_selected,"*"),
                                                get.or.blank(input$sample_selected,"*"))
    else ""
  })
  get.sample.lines<-reactive({
    log.name<-Sys.glob(papaste(get.sample.path(),"tif/*.log"))[1]
    log.file<-file(log.name,"r")
    out.lines<-readLines(log.file)
    close(log.file)
  })
  get.projections<-reactive({
    as.vector(sapply(Sys.glob(papaste(get.sample.path(),"tif/*.tif")),pa.getlast))
  })
  output$projection_selector<-renderUI({
    if (length(get.projections())>1) {
      wellPanel(h4("Projection to Read"),
                selectInput('projection_selected', 'Projections', get.projections())) 
    } else h3("No Projections for Current Sample")
  })
  
  


  

  
  ## read projections, scale, and perform flat-field correction
  read.cur.prj<-reactive({
    tif.file<-papaste(get.sample.path(),"tif",
                      get.or.blank(input$projection_selected))
    readTIFF(tif.file)
  })
  get.flt.prj<-reactive({
    "/Users/mader/Dropbox/WorkRelated/BeamlineTools/Data10/disk1/01_/tif/01_0044.tif"
  })
  read.flt.prj<-reactive({
    readTIFF(get.flt.prj())
  })
  get.cor.prj<-reactive({
    if(get.or.blank(input$flatfield_correction,F)) read.cur.prj()
    else read.flt.prj()/read.cur.prj()
  })
  get.min<-reactive({
    get.or.blank(input$min_val,0.00)
  })
  
  get.max<-reactive({
    get.or.blank(input$min_val,1)
  })
  
  output$minmax_selector<-renderUI({
    gvals<-as.vector(get.cor.prj())
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
  output$prj_histogram<-renderPlot({
    hist(get.cor.prj())
  })
  ## code for rendering the projection
  get.scaled.prj<-reactive({
    cin<-get.cor.prj()
    gvals<-cin$vals
    gvals<-(gvals-get.min())/(get.max()-get.min())
    gvals[which(gvals<0)]<-0
    gvals[which(gvals>1)]<-1
    cin$vals<-gvals
    cin
  })
  output$prj_preview<-renderPlot({
    c.img<-t(get.cor.prj())
    x.vals<-c(1:nrow(c.img))
    y.vals<-c(1:ncol(c.img)) #zlim=c(get.min(),get.max())
    image(x.vals,y.vals,c.img,useRaster=T,col=terrain.colors(100),axes=F)
    box()
  })
  


  
  ## code for previewing folders
  output$log_file<-renderTable({
    as.data.frame(get.sample.lines())
  })
  
  output$folder_contents<-renderDataTable({
    if(nchar(get.sample.path()>0)) {
      all.subfiles<-Sys.glob(papaste(get.sample.path(),"*/*.*"))
      subfiles<-ldply(all.subfiles,function(filename) {
        data.frame(folder=pa.getlast(filename,1),size=file.info(filename)$size)
      })
      ddply(subfiles,.(folder),function(c.folder) data.frame(files=nrow(c.folder),size=sum(c.folder$size)/1e6))
    }
    
  })
  
  
})