\name{vmm.predict}
\alias{vmm.predict}
\title{predict compression learning object}
\description{
  This function forecasts the probability of the symbol
  given the context for learned obj
}
\usage{
  vmm.predict(obj,context,symbol) 
}
\details{
  Details are presently unavailable, code taken from:
  http://www.cs.technion.ac.il/~ronbeg/vmm/index.html
}
\value{
 probability of symbol given the context
}
\author{Scott Locklin}
\examples{
  vmm.init() -> obj
  vmm.train(obj,'abracadabra')
  vmm.predict(obj,'ab','c')
}
\keyword{predict}