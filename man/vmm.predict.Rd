\name{vmm.predict}
\alias{vmm.predict}
\title{predict compression learning object}
\description{
  This function forecasts the probability of a symbol
  conditioned on the context for learned obj
}
\usage{
  vmm.predict(obj,context,symbol) 
}

\arguments{
  \item{obj}{VMM object created by vmm.init}
  \item{context}{the context of the prediction}
  \item{symbol}{the symbol to calculate the probability of}
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