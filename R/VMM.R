## this is a simple set of tools for using Begleiter et. al.'s VMM code.
## http://www.cs.technion.ac.il/~ronbeg/vmm/index.html
## 
## I may abstract these into an S3 class at some point, but as
## long as the user knows the java native compression learning objects
## are mutable, it should be fine for now.

ascii2int<-function(x) {
  strtoi(charToRaw(x),16L)
}

java.charseq<-function(s) {
  .jnew("java.lang.String",s) -> strob
  .jcast(strob,new.class="java.lang.CharSequence") -> out
  return(out)
}

vmm.init<-function(kind="PPMC",size=5,d=5,m=2,s=8,
                   pmin=0.006,alpha=0.0,r=1.05,gamma=0.0006) {
  ## can be LZms, PPMC, DCTW, BinaryCTW, LZ78, PST
  ## size is size of alphabet, d is size of ...?
  paste("vmm.algs.",kind,"Predictor",sep="") -> method
  .jnew(method) -> obj
  if(kind=="LZms") {
    .jcall(obj,"V","init",as.integer(size),as.integer(m),as.integer(s))
  } else if(kind=="LZ78") {
    .jcall(obj,"V","init",as.integer(size))
  } else if(kind=="DCTW") {
    .jcall(obj,"V","init",as.integer(size),as.integer(d))
  } else if(kind=="BinaryCTW") {
    .jcall(obj,"V","init",as.integer(size),as.integer(d))
  } else if(kind=="PPMC") {
    .jcall(obj,"V","init",as.integer(size),as.integer(d))
  } else if(kind=="PST") {
    .jcall(obj,"V","init",as.integer(size),as.double(pmin),as.double(alpha),
           as.double(gamma),as.double(r),as.integer(d))
  }
  return(obj)
}


vmm.train<-function(o,s){
  java.charseq(s) -> tmp
  .jcall(o,"V","learn",tmp)
}

vmm.predict<-function(o,s,ch) {
  ascii2int(ch) -> charint
  java.charseq(s) -> sj
  return(.jcall(o,"D","predict",charint,sj))
}

vmm.loglik<-function(o,s,co=FALSE) {
  java.charseq(s) -> sj
  if(class(co)=="logical") {
    .jcall(o,"D","logEval",sj) -> out
  } else {
    java.charseq(co) -> coj
    .jcall(o,"D","logEval",sj,coj) -> out
  }
  return(out)
}

