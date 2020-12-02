\name{lvglasso}
\alias{lvglasso}

\title{
Estimate Graphical Model using lvglasso method
}
\description{
Estimate graphical model with latent variables using Chandrasekaran et al. 2010.
}
\usage{
lvglasso(data, n, p, lambda1, lambda2, rule = "AND")
}

\arguments{
  \item{data}{data set, can be a matrix or data frame}
  \item{n}{the number of observations}
  \item{p}{the number of variables}
  \item{lambda1}{the first penalty value}
  \item{lambda2}{the second penalty value}
  \item{rule}{rules to combine covariance matrix. Options are "AND" and "OR".}
}

\details{
The lvglasso method assumes that all the variables, both observed and latent, are jointly Gaussian.
The method is designed to solve the following problem
\deqn{
(Sn_hat, Ln_hat) = argmin(S,L) - l(S-L;\sum_{O}^n) + \lambda_n(\gamma||S||_1 + tr(L))
s.t. S-L > 0, L \ge 0.
}
}

\value{
  \item{theta}{the resulting covariance matrix, theta. Indicating the conditional dependence relationship between the variables}
  \item{penalties}{the penalty values}
}

\references{
Chandrasekaran, V., Parrilo, P. A. & Willsky, A. S. (2010), Latent variable graphical model selection via convex optimization, in `Communication, Control, and Computing (Allerton), 2010 48th Annual Allerton Conference on', IEEE, pp. 1610--1613.
}

\author{
Rpackage contributor: Samantha Yang, Yanxin Jin
}

\examples{
#lvglasso Gaussian with "AND" rule
n <- 50
R <- 20
p <- 30
seed <- 1
l <- 2
s <- 2
data <- generate_Gaussian(n, R, p, l, s, sparsityA = 0.95, sparsityobserved = 0.9,
sparsitylatent = 0.2, lwb = 0.3, upb = 0.3, seed)$X

result <- lvglasso(data, n = 50, p = 30, lambda1 = 0.222, lambda2 = 0.1*0.222, rule = "AND")
}

