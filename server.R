library(shiny)
library(ggplot2)
library(ggthemes)
library(plyr)
source('recoTools.R')
shinyServer(function(input, output) {
  
  ## path code for selecting the current measurement
  root.dir<-reactive({
    if(!exists("old.wd")) old.wd<-getwd()
    old.wd
  })
  get.drives<-reactive({
    drv.list<-sapply(Sys.glob(pa.paste(root.dir(),"*/")),
                     pa.getlast)
    as.vector(drv.list)
  })
  get.disks<-reactive({
    as.vector(sapply(Sys.glob(pa.paste(root.dir(),
                                      get.or.blank(input$drive_selected,"*"),
                                      "*/")),
                              pa.getlast))
  })
  get.samples<-reactive({
    as.vector(sapply(Sys.glob(pa.paste(root.dir(),
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
  get.disk.path<-reactive({
    pa.paste(root.dir(),
             get.or.blank(input$drive_selected,"*"),
             get.or.blank(input$disk_selected,"*"))
  })
  # reading the log information and projections
  get.sample.path<-reactive({
    if(!is.null(input$sample_selected)) pa.paste(get.disk.path(),
                                                get.or.blank(input$sample_selected,"*"))
    else ""
  })
  get.log.lines<-reactive({
    log.name<-Sys.glob(pa.paste(get.sample.path(),"tif/*.log"))[1]
    print(log.name)
    log.file<-file(log.name,"r")
    out.lines<-readLines(log.file)
    close(log.file)
    parse.log.lines(out.lines)
  })
  get.scan.info<-reactive({
    parse.scan.info(get.log.lines())
  })
  get.projections<-reactive({
    as.vector(sapply(Sys.glob(pa.paste(get.sample.path(),"tif/*.tif")),pa.getlast))
  })
  output$projection_selector<-renderUI({
    if (length(get.projections())>1) {
      wellPanel(selectInput('projection_selected', 'Projections', get.projections()), 
      sliderInput('drk_sel', 'Select Dark', min=1, max=get.scan.info()$Darks,value=1,round=0),
      sliderInput('flt_sel', 'Select Flat', min=1, max=get.scan.info()$Flats,value=1,round=0),
      sliderInput('prj_sel', 'Select Projection', min=1, max=get.scan.info()$Projections,value=1,round=0))
    } else h3("No Projections for Current Sample")
  })
  
  ## read projections, scale, and perform flat-field correction
  read.cur.prj<-reactive({
    tif.file<-pa.paste(get.sample.path(),"tif",
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
    if(get.or.blank(input$flatfield_correction,F)) read.flt.prj()/read.cur.prj()
    else read.cur.prj()
  })
  output$prj_histogram<-renderPlot({
    hist(get.cor.prj())
  })
  
  output$minmax_selector<-renderUI({
    gvals<-as.vector(get.cor.prj())
    if (length(gvals)>1) {
      wellPanel(sliderInput('prj_min_val', 'Minimum Value',
                            min=min(gvals), max=max(gvals),
                            value=min(gvals)),
                sliderInput('prj_max_val', 'Maximum Value',
                            min=min(gvals), max=max(gvals),
                            value=max(gvals)),
                h4(paste("Cur Position",input$previewClick$x,", y",input$previewClick$y))) 
    } else h3("No Image Loaded")
  })

  get.min<-reactive({
    get.or.blank(input$prj_min_val,0.00)
  })
  
  get.max<-reactive({
    get.or.blank(input$prj_max_val,1)
  })
  
  ## code for rendering the projection
  get.colors<-reactive({
    if (input$use_color) rainbow(80)
    else gray((0:80)/80)
  })
  output$prj_preview<-renderPlot({
    c.img<-t(get.cor.prj())
    x.vals<-c(1:nrow(c.img))
    y.vals<-c(1:ncol(c.img)) 
    if(input$prj_crop) {
      imagefn<-function(...) image(...,xlim=c(input$prj_crop_minx,input$prj_crop_maxx),
                                   ylim=c(input$prj_crop_miny,input$prj_crop_maxy),asp=0,axes=T) # no aspect ratio
    }
    else {
      imagefn<-function(...) image(...,asp=1,axes=F)
    }
    
    imagefn(x.vals,y.vals,c.img,
          zlim=c(get.min(),get.max()),
          useRaster=T,col=get.colors())
  })
  
  
  
  ## reconstruction previewing and such
  get.reconstructions<-reactive({
    as.vector(sapply(Sys.glob(pa.paste(get.sample.path(),"rec*/")),pa.getlast))
  })
  get.reco.slices<-reactive({
    as.vector(sapply(Sys.glob(pa.paste(get.sample.path(),
                                       get.or.blank(input$reconstruction_selected),
                                       "/*.*")),pa.getlast))
  })
  output$reconstruction_selector<-renderUI({
    if (length(get.reconstructions())>1) {
      wellPanel(selectInput('reconstruction_selected', 'Reconstruction', get.reconstructions()), 
      selectInput('slice_selected','Slice',get.reco.slices()))
    } else h3("No Reconstructions for Current Sample")
  })
  
  
  ## code for previewing folders
  output$log_file<-renderDataTable({
    o.df<-(get.log.lines())
    o.df
  })
  
  output$folder_contents<-renderDataTable({
    if(nchar(get.sample.path()>0)) {
      all.subfiles<-Sys.glob(pa.paste(get.sample.path(),"*/*.*"))
      subfiles<-ldply(all.subfiles,function(filename) {
        data.frame(folder=pa.getlast(filename,1),
                   ext=get.last(strsplit(pa.getlast(filename),"[.]")[[1]]),
                   size=file.info(filename)$size)
      })
      ddply(subfiles,.(folder),function(c.folder) {
        data.frame(files=nrow(c.folder),
                   size_gb=sum(c.folder$size)/1e9,
                   exts=paste(unique(c.folder$ext),collapse=","))
      })
    }
    
  })
  
  output$disk_contents<-renderDataTable({
    if(nchar(get.disk.path()>0)) {
      all.subfiles<-Sys.glob(pa.paste(get.disk.path(),"*/*/*.*"))
      subfiles<-ldply(all.subfiles,function(filename) {
        data.frame(folder=pa.getlast(filename,3),
                   ext=get.last(strsplit(pa.getlast(filename),"[.]")[[1]]),
                   size=file.info(filename)$size)
      })
      
      ddply(subfiles,.(folder),function(c.folder) {
        data.frame(files=nrow(c.folder),
                   size_gb=sum(c.folder$size)/1e9,
                   exts=paste(unique(c.folder$ext),collapse=","))
        })
    }
    
  })
  
  
})