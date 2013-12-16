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
read.dmp.vals<-function(path="/Users/mader/Dropbox/TIPL/test/io_tests/rec_DMP/pivoB04_0301.sin.DMP") {
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
read.dmp<-function(path="/Users/mader/Dropbox/TIPL/test/io_tests/rec_DMP/pivoB04_0301.sin.DMP") {
  dmp.val<-read.dmp.vals(path)
  dmp.val.to.img(path)
}

pprint<-function(...) print(paste(...))
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
  o.df<-ldply(fl,function(c.item) {
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
parse.scan.info<-function(in.df) {
  o.val<-list()
  o.val$Darks<-as.numeric(subset(in.df,Category=="Scan Settings"  & Option=="Number of darks")$Value)
  o.val$Flats<-as.numeric(subset(in.df,Category=="Scan Settings"  & Option=="Number of flats")$Value)
  o.val$Projections<-as.numeric(subset(in.df,Category=="Scan Settings"  & Option=="Number of projections")$Value)
  o.val
}

