\name{corlatent}
\alias{corlatent}

\title{
Estimate Graphical Model using corlatent method
}
\description{
Estimate graphical model with latent variables and correlated replicates using the corlatent method (Yanxin 2020).
}
\usage{
corlatent(data, accuracy, n, R, p, lambda1, lambda2, lambda3, distribution = "Gaussian",
rule = "AND")
}

\arguments{
  \item{data}{data set, can be a matrix, list, array, or data frame}
  \item{accuracy}{how much the error should be minimized}
  \item{n}{the number of observations}
  \item{R}{the number of replicates for each observation}
  \item{p}{the number of variables}
  \item{lambda1}{the first penalty value}
  \item{lambda2}{the second penalty value}
  \item{lambda3}{the third penalty value}
  \item{distribution}{For a data set with Gaussain distribution, use "Gaussian"; For a data set with Ising distribution, use "Ising".}
  \item{rule}{rules to combine covariance matrix. Options are "AND" and "OR".}
}

\details{
The corlatent method has two assumptions. Assumption 1 states that the R replicates are assumed to follow a one-lag stationary vector autoregressive model, conditioned on the unmeasured confounders. In addition, the observed variables are conditionally independent of the unmeasured confounders across different replicates.
Assumption 2 states that the unmeasured confounders are piecewise constant across replicates.
Based on these two assumptions, the method solve the following problem
\deqn{
(\hat{\theta}_{j,-j}, \hat{\alpha}_j, \hat{\Delta}_j) = argmin -l(\theta_{j,-j}, \alpha_j, \Delta_j) + \lambda||\theta_{j,-j}||_1 + \beta||\alpha_j||_1 + \gamma||(I_n otimes C)\Delta_j||_1,
}
where where \eqn{l(\theta_{j,-j}, \alpha_j, \Delta_j) = \sum_{i=1}^n \sum_{t=1}^T log p(x_itj | x_it(-j), x_i(t-1)j, \Delta_itj)/(nR), \lambda, \beta, \gamma} are the tuning parameters, I_n is a n-dimensional identical matrix and C is the discrete first derivative matrix where the i and i+1 column of every ith row are -1 and 1, respectively.
This method aims at modeling exponential family graphical models with correlated replicates and unmeasured confounders.
}

\value{
  \item{theta}{the resulting covariance matrix, theta. Indicating the conditional dependence relationship between the variables}
  \item{penalties}{the penalty values}
}

\references{
Yanxin Jin, Yang Ning, and Kean Ming Tan (2020), `Exponential Family Graphical Models with Correlated Replicates and Unmeasured Confounders', in preparation.
}

\author{
Rpackage contributor: Samantha Yang, Yanxin Jin
}

\examples{
# corlatent Gaussian with "AND" rule
n <- 20
R <- 10
p <- 5
seed <- 1
l <- 2
s <- 2

data <- generate_Gaussian(n, R, p, l, s, sparsityA = 0.95, sparsityobserved = 0.9,
sparsitylatent = 0.2, lwb = 0.3, upb = 0.3, seed)$X

result <- corlatent(data, accuracy = 1e-6, n, R, p,
lambda1 = 0.01, lambda2 = 1e+5, lambda3 = 1e+5,distribution = "Gaussian", rule = "AND")

result$theta # This is the graph
result$penalties # This is the penalty values
}
