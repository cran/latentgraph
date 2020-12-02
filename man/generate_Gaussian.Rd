\name{generate_Gaussian}
\alias{generate_Gaussian}

\title{
Generate a Gaussian distributed data set
}

\description{
This function will generate a Gaussian distributed data set.
}

\usage{
generate_Gaussian(n, R, p, l, s, sparsityA, sparsityobserved,
sparsitylatent, lwb, upb, seed)
}

\arguments{
  \item{n}{the number of observations}
  \item{R}{the number of replicates}
  \item{p}{the number of variables}
  \item{l}{the number of latent variables}
  \item{s}{latent variables change (s-1) times across replicates. s should be a factor of T.}
  \item{sparsityA}{sparsity of transition matrix A}
  \item{sparsityobserved}{sparsity for the covariance of the observed variables}
  \item{sparsitylatent}{sparsity for the covariance of the latent variables}
  \item{lwb}{lower bound for the elements in the covariance matrix}
  \item{upb}{upper bound for the elements in the covariance matrix}
  \item{seed}{the seed for the random number generator}
}

\details{
This function aims to generate a Gaussian distributed data set, and this data has AR(1) process, where the transition matrix A is sparse with 5 percent of the elements of A set equal to 0.3 as default. The jth variable at the tth replicate may be conditionally dependent with other variables at the (t-1)th replicate.
\eqn{\Sigma} is the covariance matrix for the observed variables X and the unmeasured confounders U, and we partition \eqn{\Sigma} into matrices that quantify the relationships among the observed variables (\eqn{\Sigma_XX}), between the observed variables and unmeasured confounders (\eqn{\Sigma_XU} or \eqn{\Sigma_UX}), and of the unmeasured confounders (\eqn{\Sigma_UU}).
In general, the data is generated with:
\deqn{
X_(i1) | U_(i1) ~ N_p{\Sigma_(OH)\Sigma^(-1)_(HH)U_(i1), \Sigma_(XX) - \Sigma_(XU)\Sigma^(-1)_(UU)\Sigma_(UX)},
}

\deqn{
X_(it) | X_{i(t-1)},U_(it) ~ N_p[AX_{i(t-1)} + \Sigma_(OH)\Sigma^(-1)_(HH)U_(it), \Sigma_(XX) - \Sigma_(XU)\Sigma^(-1)_(UU)\Sigma_(UX)].
}
}

\value{
  \item{X}{the generated data matrix}
  \item{truegraph}{the true graph of the observed variables}
}

\references{
Yanxin Jin, Yang Ning, and Kean Ming Tan (2020), `Exponential Family Graphical Models with Correlated Replicates and Unmeasured Confounders', in preparation.
}

\author{
Rpackage contributor: Samantha Yang, Yanxin Jin
}

\examples{
n <- 50
R <- 20
p <- 30
seed <- 1
l <- 2
s <- 2
data <- generate_Gaussian(n, R, p, l, s, sparsityA = 0.95, sparsityobserved = 0.9,
sparsitylatent = 0.2, lwb = 0.3, upb = 0.3, seed)
}
