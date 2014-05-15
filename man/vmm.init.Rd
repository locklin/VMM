\name{vmm.init}
\alias{vmm.init}
\title{create compression learning object}
\description{
  This function creates a compression learning object
  of kind LZms, PPMC, DCTW, BinaryCTW, LZ78, PST
}
\usage{
  vmm.init(kind="PPMC",size=128,d=5,m=2,s=8,pmin=0.006,alpha=0.0,r=1.05,gamma=0.0006)->obj
}

\arguments{
  \item{kind}{name of tree type: LZms, PPMC, DCTW, BinaryCTW, LZ78,
    PST; defaults to LZ78}
  \item{size}{size of tree; defaults to 128}
  \item{d}{d, defined for all except LZ78}
  \item{m}{defined for LZms}
  \item{s}{defined for LZms}
  \item{pmin}{defined for PST}
  \item{alpha}{defined for PST}
  \item{r}{defined for PST}
  \item{gamma}{defined for PST}
  }
  \details{
    Mostly paraphrasing Begleiter et al:
    LZ78 starts with empty phrase, and each step parses the shortest
    phrase not already in the dictionary. Tree notes retain a count of
    phrase occurrences. To compute P(x|s), start from root, and traverse
    the tree.
    PPM has an upper bound 'd' on the maximal Markov order of the VMM
    it constructs. PPM constructs probability fo rall symbols that do
    not appear after the context s, and the rest of the probability is
    distributed among the smbols which do appear.
    CTW again has a 'd' max Markov order bound. It is a mixture of a
    collection of bounded models.
    PST again, has 'd' max Markov: learning is O(Dn^2) with space
    complexity O(Dn) and P(x|s) O(D)
    LZ-MS has params 'm' and 's.' 's' is input shifting, where the
    training is parsed s+1 times, with shifted sequence. s=0 corresponds
    to LZ78. M does the same thing with back-shifting. Both improve
    stats. Complexity is m*s*O(LZ78)
    Code taken from:
    http://www.cs.technion.ac.il/~ronbeg/vmm/index.html
}
\value{
  An object is initialized and returned for training.
}
\author{Scott Locklin}
\examples{
  vmm.init("LZms",256) -> obj
  vmm.train(obj,'abracadabra')
  vmm.predict(obj,'ab','c')
}
\keyword{init}
