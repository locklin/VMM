\name{vmm.loglik}
\alias{vmm.loglik}
\title{log likelihood of sequence given compression learning object}
\description{
  This function gives the log likelihood of a sequence
  given the learned obj and an optional context
}
\usage{
  vmm.loglik(obj,sequence)
  vmm.loglik(obj,sequence,context) 
}
\details{
  Details are presently unavailable, code taken from:
  http://www.cs.technion.ac.il/~ronbeg/vmm/index.html
}
\value{
 probability of sequence given the learned obj
}
\author{Scott Locklin}
\examples{
  vmm.init() -> obj
  vmm.train(obj,'abracadabra')
  vmm.loglik(obj,'ab')
  vmm.loglik(obj,'ab','aaaa')
}
\keyword{loglik}
