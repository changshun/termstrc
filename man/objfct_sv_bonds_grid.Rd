\name{objfct_sv_bonds_grid}
\alias{objfct_sv_bonds_grid}
\title{
Svensson Grid Loss Function for Bonds 
}
\description{
Calculates the sum of the weighted squared price error.
}
\usage{
objfct_sv_bonds_grid(beta, tau, m, cf, w, p)
}

\arguments{
  \item{beta}{Beta parameters of the Svensson spot price function}
  \item{tau}{Tau parameters of the Svensson spot price function}
  \item{m}{maturities matrix}
  \item{cf}{cash flows matrix}
  \item{w}{weights vector}
  \item{p}{price vector}
}

