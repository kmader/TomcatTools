# fixed dataset
c.nums<-c("30","35","40","45","50","55")
c.files<-paste("http://people.ee.ethz.ch/~maderk/pivoB04_0",c.nums,"1.sin.DMP",sep="")
names(c.files)<-c.nums
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