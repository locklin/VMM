\name{vmm.loglik}
\alias{vmm.loglik}
\title{log likelihood of sequence given compression learning object}
\description{
  This function gives the base-2 log likelihood of a sequence
  given the learned obj and an optional context
  
}
\usage{
  vmm.loglik(obj,sequence)
  vmm.loglik(obj,sequence,context) 
}
\arguments{
  \item{obj}{VMM object created by vmm.init}
  \item{sequence}{the sequence to calculate the log likelihood of}
  \item{context}{the context of the log likelihood; optional}
}

\details{
  Details are presently unavailable, Java code taken from Ron Begleiter's website:
  http://www.cs.technion.ac.il/~ronbeg/vmm/index.html
}
\value{
 log likelihood of sequence given the learned obj
}
\author{Scott Locklin}
\examples{
  vmm.init() -> obj
  vmm.train(obj,'abracadabra')
  vmm.loglik(obj,'ab')
  vmm.loglik(obj,'ab','aaaa')
}
\keyword{loglik}
