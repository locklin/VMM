\name{vmm.train}
\alias{vmm.train}
\title{trains compression learning object}
\description{
  This function adds data to the compression learning object. Since it
  is an online learner, you can add as you go, and do predictions.
  The function doesn't return anything: the compression learning object
  is changed sub-rosa.
}
\usage{
  vmm.train(obj,sequence) 
}
\details{
  Details are presently unavailable, code taken from:
  http://www.cs.technion.ac.il/~ronbeg/vmm/index.html
}
\value{
 There is no returned value, though it might return an error message.
}
\author{Scott Locklin}
\examples{
  vmm.init() -> obj
  vmm.train(obj,'abracadabra')
  vmm.loglik(obj,'ab')
  vmm.train(obj,'abracadacada')
  vmm.loglik(obj,'ab')
}
\keyword{train}