\name{predict.misrepEM}
\alias{predict.misrepEM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict method for 'misrepEM' Model Fits
}
\description{
Predicted values based on a fitted 'misrepEM' model object.
}
\usage{
## S3 method for class 'misrepEM'
predict.misrepEM(object, newdata, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{object}{a fit from one of \code{gammaRegMisrepEM}, \code{LnRegMisrepEM}, \code{NormRegMisrepEM}, \code{nbRegMisrepEM}, or \code{poisRegMisrepEM}.}

  \item{newdata}{a data frame containing predictors that are to be used to make predictions of the response. }
  \item{\dots}{currently not used.}
}
\details{
Currently, only predictions made on the scale of the response variable are supported.

Incomplete cases are automatically dropped, and predictions are made only on complete cases.

}
\value{
\code{predict.misrepEM} returns a numeric vector of predictions.
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

# Split data into training and testing sets
train <- data[1:1800,]
test  <- data[1801:2000,]

gamma_fit <- gammaRegMisrepEM(formula = Y ~ X1 + X2 + X3 + V_star,
                              v_star = "V_star", data = train)

# Predict on test set;
preds <- predict(gamma_fit, newdata = test)

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
