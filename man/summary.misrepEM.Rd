\name{summary.misrepEM}
\alias{summary.misrepEM}
\alias{print.summary.misrepEM}
\title{
Summarize a 'misrepEM' Model Fit
}
\description{
\code{summary} method for class 'misrepEM'.
}
\usage{
\method{summary}{misrepEM}(object, \dots)

\method{print}{summary.misrepEM}(x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
an object of class "\code{misrepEM}", usually resulting from a call to one of \code{\link{gammaRegMisrepEM}}, \code{\link{LnRegMisrepEM}}, \code{\link{NormRegMisrepEM}}, \code{\link{nbRegMisrepEM}} or \code{\link{poisRegMisrepEM}}.
}

  \item{x}{
an object of class "\code{summary.misrepEM}", usually resulting from a call to \code{summary.misrepEM}.
  }

  \item{\dots}{currently not used.}


}

\value{
\code{summary.misrepEM} returns an object of class \code{"summary.misrepEM"}, a list of length 5 with the following components:

\item{coefficients}{a \code{data.frame} of coefficients, standard errors, standardized coefficients, two-tailed p-values corresponding to the standardized coefficient based on a Student-t or Normal reference distribution, and 'significance stars.'}

\item{ICs}{a named numeric vector of length three, containing the Akaike Information Criterion (AIC), the corrected AIC (AICc) and the Bayesian Information Criterion (BIC).}

\item{loglik}{numeric. The log-likelihood of the fitted \code{misrepEM} model.}

\item{lambda}{numeric. The estimated prevalence of misrepresentation.}

\item{lambda_stderror}{numeric. The standard error of the estimated prevalence of misrepresentation.}

}

\references{
Xia, Michelle, Rexford Akakpo, and Matthew Albaugh. "Maximum Likelihood Approaches to Misrepresentation Models in GLM ratemaking: Model Comparisons." \emph{Variance} 16.1 (2023).

Akakpo, R. M., Xia, M., & Polansky, A. M. (2019). Frequentist inference in insurance ratemaking models adjusting for misrepresentation. \emph{ASTIN Bulletin: The Journal of the IAA, 49}(1), 117-146.

Xia, M., Hua, L., & Vadnais, G. (2018). Embedded predictive analysis of misrepresentation risk in GLM ratemaking models. \emph{Variance, 12}(1), 39-58.
}

\examples{

# Simulate data
n <- 2000
p0 <- 0.25

X1 <- rbinom(n, 1, 0.4)
X2 <- rnorm(n, 0, 1)
X3 <- rbeta(n, 2, 1)

theta0 <- 0.3
V <- rbinom(n,1,theta0)
V_star <- V
V_star[V==1] <- rbinom(sum(V==1),1,1-p0)

a0 <- 1
a1 <- 2
a2 <- 0
a3 <- 4
a4 <- 2

mu <- exp(a0 + a1*X1 + a2*X2 + a3*X3 + a4*V)

phi <- 0.2
alpha0 <- 1/phi
beta <- 1/mu/phi
Y <- rgamma(n, alpha0, beta)

data <- data.frame(Y = Y, X1 = X1, X2 = X2, X3 = X3, V_star = V_star)

gamma_fit <- gammaRegMisrepEM(formula = Y ~ X1 + X2 + X3 + V_star,
                              v_star = "V_star", data = data)

summary(gamma_fit)

# Coefficients:
#             Estimate Std. Error  t value Pr(>|t|)
# (Intercept)  1.00137    0.03413 29.33857   <2e-16 ***
# X1           2.01388    0.02154 93.48440   <2e-16 ***
# X2          -0.00193    0.01038 -0.18589  0.85255
# X3           4.00101    0.04560 87.74528   <2e-16 ***
# V_star       2.00567    0.02240 89.54515   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ---
#      AIC     AICc      BIC
# 23362.50 23362.56 23401.71
# ---
# Log-Likelihood
#      -11674.25
# ---
# Lambda:  0.09635239 std.err:  0.007641834

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
