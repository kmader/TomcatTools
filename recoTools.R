library(tiff)
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
