\name{vmm.init}
\alias{vmm.init}
\title{create compression learning object}
\description{
  This function creates a compression learning object
  of kind LZms, PPMC, DCTW, BinaryCTW, LZ78, PST
}
\usage{
  vmm.init("LZms",256,5) -> obj
}
\details{
  Details are presently unavailable, code taken from:
  http://www.cs.technion.ac.il/~ronbeg/vmm/index.html
}
\value{
  An object is returned for training.
}
\author{Scott Locklin}
\examples{
  vmm.init() -> obj
  vmm.train(obj,'abracadabra')
  vmm.predict(obj,'ab','c')
}
\keyword{init}