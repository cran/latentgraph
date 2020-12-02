\name{semilatent}
\alias{semilatent}

\title{
Estimate Graphical Model using semilatent method
}
\description{
Estimate graphical model with latent variables and independent replicates using the semilatent method (Tan et al.(2016)).
}
\usage{
semilatent(data, n, R, p, lambda, distribution = "Gaussian", rule = "AND")
}

\arguments{
  \item{data}{data set, can be a matrix, list, array, or data frame}
  \item{n}{the number of observations}
  \item{R}{the number of replicates for each observation}
  \item{p}{the number of variables}
  \item{lambda}{the penalty value}
  \item{distribution}{For a data set with Gaussain distribution, use "Gaussian"; For a data set with Ising distribution, use "Ising".}
  \item{rule}{rules to combine covariance matrix. Options are "AND" and "OR".}
}

\details{
The semilatent method has two assumptions. The first one states that the latent variables are constant across replicates.
Assumption 2 states that given the latent variables, the R replicates are mutually independent.
With these two assumptions, the method seek to solve the following problem.
\deqn{
(B_j,O\j_hat) = argmin {l_j(B_j,O\j) + \lambda||B_j,O\j||_1}
}
where \eqn{\lambda} is a non-negative tuning parameter that controls the sparsity of the estimate \eqn{B_j,O\j_hat}.
This method aims at modeling semiparametric exponential family graphical model with latent variables.
}

\value{
  \item{theta}{the resulting covariance matrix, theta. Indicating the conditional dependence relationship between the variables}
  \item{penalty}{the penalty value}
}

\references{
Tan, K. M., Ning, Y., Witten, D. M. & Liu, H. (2016), `Replicates in high dimensions, with applications to latent variable graphical models', Biometrika 103(4), 761--777.
}

\author{
Rpackage contributor: Samantha Yang, Yanxin Jin
}

\examples{
#semilatent Gaussian with "AND" rule
n <- 50
R <- 20
p <- 30
seed <- 1
l <- 2
s <- 2
data <- generate_Gaussian(n, R, p, l, s, sparsityA = 0.95, sparsityobserved = 0.9,
sparsitylatent = 0.2, lwb = 0.3, upb = 0.3, seed)$X

result <- semilatent(data, n, R, p, lambda = 0.1,
distribution = "Gaussian", rule = "AND")
}

