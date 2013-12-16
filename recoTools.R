library(tiff)
library(gdata)
# path paste
pa.paste<-function(...) paste(...,sep="/")
get.last<-function(in.list,offset=0) in.list[length(in.list)-offset]
pa.getlast<-function(in.path,offset=0) get.last(strsplit(in.path,"/")[[1]],offset=offset)
get.or.blank<-function(var,altval="") {
  if(is.null(var)) altval
  else var
}
read.dmp.vals<-function(path) {
  pprint("reading img",path)
  if(nchar(path)>1) {
    cdmp<-file(path,open="rb")
    dimx<-readBin(cdmp,"int",signed=F,endian="little",size=2)
    dimy<-readBin(cdmp,"int",signed=F,endian="little",size=2)
    dimz<-readBin(cdmp,"int",signed=F,endian="little",size=2)
    o.vals<-readBin(cdmp,"double",endian="little",size=4,n=dimx*dimy)
    close(cdmp)
  } else {
    print("image is empty")
    o.vals<-c(1,1,1,1)
    dimx<-2
    dimy<-2
  }
  list(vals=o.vals,dim=c(dimx,dimy))
}
dmp.val.to.img<-function(dmp.val) {
  (array(dmp.val$vals,dim=dmp.val$dim))
}
#' Read DMP loads a dmp file as a data frame
#'
#' Data frame is a list of values (vals) and a dimension (dim=(x,y)) which can be used with reshape
#' to regenerate a matrix 
#'
#' @param path the name of the DMP file to open
read.dmp<-function(path) {
  dmp.val<-read.dmp.vals(path)
  dmp.val.to.img(path)
}

pprint<-function(...) print(paste(...)) # makes print and paste more convienent
#' Parse the log file made by the scanning and reconstruction at TOMCAT
#'
#' The command cleans the whitespace at the beginning and end (using trim, ideally gsub would work)
#' it then splits all lines into option (before the :) and value (after the :)
#' it assigns catagories based on intermediate lines starting with ---
#'
#' @param log.lines is a list with an entry for each line
parse.log.lines<-function(log.lines) {
  get.clean<-function(c.arg) { # trim the leading and trailing whitespaces
    gsub(". $", "",gsub("^ .", "", c.arg))
  }
  get.clean<-trim
  get.val<-function(c.in) {
    if(length(c.in)<2) ""
    else get.clean(get.last(c.in))
  }
  # initially no category
  cur.category<<-""
  o.df<-ldply(log.lines,function(c.item) {
    if(substring(c.item,1,3)=="---") {
      # new catagory
      invisible(cur.category<<-get.clean(gsub("-","",c.item)))
      data.frame()
    } else {
      if(nchar(c.item)>0) {
        c.split<-strsplit(c.item,":")[[1]]
        data.frame(Category=cur.category,
                   Option=get.clean(c.split[1]),
                   Value=get.val(c.split))
      } else {
        data.frame()
      }
    }
  })
  o.df$Value<-as.character(o.df$Value)
  o.df$Option<-as.character(o.df$Option)
  o.df
}
#' Extract scan info from the parsed log file
#'
#' Extracts specific metrics like dark, flat, and projection count from the parsed data frame
#'
#' @param in.df the data frame produced by parse.log.lines
parse.scan.info<-function(in.df) {
  o.val<-list()
  o.val$Darks<-as.numeric(subset(in.df,Category=="Scan Settings"  & Option=="Number of darks")$Value)
  o.val$Flats<-as.numeric(subset(in.df,Category=="Scan Settings"  & Option=="Number of flats")$Value)
  o.val$Projections<-as.numeric(subset(in.df,Category=="Scan Settings"  & Option=="Number of projections")$Value)
  o.val
}