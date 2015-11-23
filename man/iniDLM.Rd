\name{iniDLM}
\alias{iniDLM}
\title{Initialize the parameters for the DLM used for a specific ration.}
\usage{
iniDLM(tMax = 12, k2 = 1.549, FF = matrix(data = c(1, 0, 0.044, k2), ncol
  = 2), GG = matrix(c(1, 0, 1, 1), nrow = 2), V = matrix(c(0.25, 0.013,
  0.013, 0.73), ncol = 2), W = matrix(c(2.194, 1.167, 1.167, 1.601), ncol =
  2), m0 = matrix(c(26.49, 5.85), nrow = 2), C0 = matrix(c(4.26, 0.32, 0.32,
  0.53), ncol = 2))
}
\arguments{
  \item{tMax}{Maximum growth period in weeks.}

  \item{k2}{Value of \eqn{k_2} in the \eqn{F} matrix.}

  \item{FF}{The \eqn{F} matrix.}

  \item{GG}{The \eqn{G} matrix.}

  \item{V}{The \eqn{V} matrix.}

  \item{W}{The \eqn{W} matrix.}

  \item{m0}{Prior latent mean \eqn{m_0}.}

  \item{C0}{Prior latent covariance matrix \eqn{C_0}.}
}
\value{
A list containing all the parameters
}
\description{
Initialize the parameters for the DLM used for a specific
ration.
}
\author{
Reza Pourmoayed \email{rpourmoayed@econ.au.dk} and Lars
Relund \email{lars@relund.dk}
}

