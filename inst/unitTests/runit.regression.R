### --- Test setup ---
library("RUnit")
library("rJava"
library("VMM")
## this presently doesn't work because of some horrific Java issue
## I don't want to think about presently
## the tests do tie out manually though

.Machine$double.eps^0.5  -> toler
'abracadabra' -> training
'ab' -> context
c('a','b','c','d','r') -> symb
 
### --- Test functions ---
 
test.lz78.reg <- function() {
  vmm.init("LZ78",size=16) -> obj
  vmm.train(obj,training)
  sapply(symb,function(x) {vmm.predict(obj,context,x)}) -> out
  checkEqualsNumeric(out,rep(0.0625,length(symb)),  tolerance=toler)
}


test.LZms.reg <- function(){
  vmm.init("LZms",size=16,m=2,s=8) -> obj
  vmm.train(obj,training)
  sapply(symb,function(x) {vmm.predict(obj,context,x)}) -> out
  checkEqualsNumeric(out,c( 0.02040816, 0.02040816, 0.02040816,
                           0.02040816, 0.69387755),  tolerance=2*toler)
}



test.DCTW.reg <- function(){
  vmm.init("DCTW",size=127,d=5) -> obj
  vmm.train(obj,training)
  sapply(symb,function(x) {vmm.predict(obj,context,x)}) -> out
  checkEqualsNumeric(out,c(0.03103914, 0.05575353, 0.01996800,
                           0.01442133, 0.87327133),  tolerance=2*toler)
}

test.BinaryCTW.reg <- function(){
  vmm.init("BinaryCTW",size=128,d=5) -> obj
  vmm.train(obj,training)
  sapply(symb,function(x) {vmm.predict(obj,context,x)}) -> out
  checkEqualsNumeric(out,c(0.09731846, 0.04567061, 0.04035459,
                           0.01807158, 0.07597322),  tolerance=4*toler)
}

test.PPMC.reg <- function(){
  vmm.init("PPMC",size=128,d=5) -> obj
  vmm.train(obj,training)
  sapply(symb,function(x) {vmm.predict(obj,context,x)}) -> out
  checkEqualsNumeric(out,c(0.018518519, 0.007407407, 0.007407407,
                           0.007407407, 0.500000000 ),  tolerance=4*toler)
}

test.PST.reg <- function(){
  vmm.init("PST",size=128) -> obj
  vmm.train(obj,training)
  sapply(symb,function(x) {vmm.predict(obj,context,x)}) -> out
  checkEqualsNumeric(out,c(0.6,   0.6,   0.6,   0.6, 923.8)/1000,tolerance=toler)
}


