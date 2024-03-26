\name{nbRegMisrepEM}
\alias{nbRegMisrepEM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit a Negative Binomial Misrepresentation Model using EM Algorithm
}
\description{
\code{nbRegMisrepEM} is used to fit a Negative Binomial regression model, adjusting for misrepresentation on a binary predictor. The function uses the Expectation Maximization algorithm and allows multiple additional correctly measured independent variables in the Negative Binomial regression with a log-link function that is typically used in insurance claims modeling. Standard errors of model estimates are obtained from closed form expressions of the Observed Fisher Information.
}
\usage{
nbRegMisrepEM(formula, v_star, data, lambda = c(0.6,0.4),
                 epsilon = 1e-08, maxit = 10000,
                 maxrestarts = 20, verb = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{formula}{an object of class "\code{\link{formula}}" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.}

\item{v_star}{a character specifying the name of the binary predictor that is suspected of being misrepresented.}

\item{data}{a dataframe containing the variables in the model.}

\item{lambda}{initial mixing proportions used to start the EM algorithm. A numeric vector of length two, with the second element being the prevelance of misrepresentation.}

\item{epsilon}{tolerance for convergence. Convergence is reached when the log-likelihood increases by less than epsilon.}

\item{maxit}{the maximum number of iterations the EM routine will run for.}

\item{maxrestarts}{how many times the EM routine will attempt to converge. When conergence is not achieved, the EM routine restarts with new randomly selected mixing proportions.}

\item{verb}{logical. If TRUE, the difference in new .vs. old log-likelihood and the current log-likelihood is printed to the console after every iteration. If TRUE, the user will also be notifed if the EM algorithm must restart with new mixing proportions.}

}
\details{

Models for \code{nbRegMisrepEM} are specified symbolically. Like the \code{lm} and \code{glm} functions, the model has the form \code{response ~ terms}, where \code{response} is the numeric response vector and \code{terms} is a series of terms which specifies a linear predictor for \code{response}.

Currently, formula specification can accommodate the following expressions:

\itemize{
\item{transformations of the response: \code{log(y) ~ x}}
\item{polynomial terms: \code{y ~ x + I(x^2)}}
\item{interactions: \code{y ~ x*z}}
}

Including an offset term (e.g. \code{y ~ x + offset()}) is currently not supported.

}
\value{
\code{nbRegMisrepEM} returns an object of \code{\link{class}} \code{"misrepEM"}.

The function \code{summary} is used to obtain and print a summary of the results.

An object of class \code{"misrepEM"} is a list containing the following 14 elements:

\item{y}{the response used.}

\item{lambda}{numeric. The estimated prevelance of misrepresentation.}

\item{params}{a numeric vector containing the estimated parameters.}

\item{loglik}{the final maximized log-likelihood.}

\item{posterior}{a numeric vector. The posterior probability that the \emph{i-th} observation is not misrepresented for observations where the suspected misrepresented variable is zero, based on the last iteration of the EM algorithm. The values are not meaningful for observations where the suspected misrepresented variable is one.}

\item{all.loglik}{a numeric vector containing the log-likelihood at every iteration.}

\item{cov.estimates}{the inverse of the observed fisher information matrix evaluated at the maximum likelihood estimates.}

\item{std.error}{a numeric vector containing the standard errors of regression coefficients.}

\item{t.values}{a numeric vector containing the standardized regression coefficients.}

\item{p.values}{a numeric vector containing the \emph{p-}values of the regression coefficients.}

\item{ICs}{a numeric vector of length three containing the AIC, AICc, and BIC.}

\item{ft}{a character containing the name of the function.}

\item{formula}{an object of class \code{formula} indicating the model that was fit.}

\item{v_star_name}{a character containing the name of the binary predictor suspected of misrepresentation.}

}
\references{
Xia, Michelle, Rexford Akakpo, and Matthew Albaugh. "Maximum Likelihood Approaches to Misrepresentation Models in GLM ratemaking: Model Comparisons." \emph{Variance} 16.1 (2023).

Akakpo, R. M., Xia, M., & Polansky, A. M. (2019). Frequentist inference in insurance ratemaking models adjusting for misrepresentation. \emph{ASTIN Bulletin: The Journal of the IAA, 49}(1), 117-146.

Xia, M., Hua, L., & Vadnais, G. (2018). Embedded predictive analysis of misrepresentation risk in GLM ratemaking models. \emph{Variance, 12}(1), 39-58.
}

\examples{

\donttest{
set.seed(314159)

# Simulate data
n <- 1000
p0 <- 0.25

X1 <- rbinom(n, 1, 0.4)
X2 <- sample(x = c("a", "b", "c"), size = n, replace = TRUE)
X3 <- rnorm(n, 0, 1)

theta0 <- 0.3
V <- rbinom(n,1,theta0)
V_star <- V
V_star[V==1] <- rbinom(sum(V==1),1,1-p0)

a0 <- 1
a1 <- 2
a2 <- 0
a3 <- -1
a4 <- 4
a5 <- 2

mu <- rep(0, n)

for(i in 1:n){

  mu[i] <- exp(a0 + a1*X1 + a4*X3 + a5*V )[i]

  if(X2[i] == "a" || X2[i] == "b"){

    mu[i] <- mu[i]*exp(a2)

  }else{
    mu[i] <- mu[i]*exp(a3)
  }

}

Y <- rnbinom(n, size = 1, mu = mu)

data <- data.frame(Y = Y, X1 = X1, X2 = X2, X3 = X3, V_star = V_star)

# "a" is the reference
data$X2 <- as.factor(data$X2)

# Model with main effects:
NB_mod <- nbRegMisrepEM(formula = Y ~ X1 + X2 + X3 + V_star,
                        v_star = "V_star", data = data)

# The prevalence of misrepresentation;
(theta0 * p0) / (1 - theta0*(1-p0)) # 0.09677419

# Parameter estimates and estimated prevalence of
# misrepresentation (lambda);
summary(NB_mod)

# Coefficients:
#             Estimate Std. Error  t value Pr(>|t|)
# (Intercept)  0.94091    0.10797  8.71423   <2e-16 ***
# X1           2.03485    0.09517 21.38182   <2e-16 ***
# X2b          0.13346    0.10998  1.21356  0.22521
# X2c         -0.96514    0.11629 -8.29914   <2e-16 ***
# X3           4.07667    0.05874 69.40599   <2e-16 ***
# V_star       1.90011    0.09517 19.96485   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ---
#      AIC     AICc      BIC
# 7661.457 7661.602 7700.719
# ---
# Log-Likelihood
#      -3822.728
# ---
# Lambda:  0.093119 std.err:  0.02233344


# Fitting an interaction between X2 and X3;

a6 <- -2
a7 <- 2

for(i in 1:n){

  if(X2[i] == "c"){
    mu[i] <- mu[i]*exp(a6*X3[i])
  }else{
    if(X2[i] =="b"){
      mu[i] <- mu[i]*exp(a7*X3[i])
    }
  }
}

Y <- rnbinom(n, size = 1, mu = mu)

data$Y <- Y

NB_mod <- nbRegMisrepEM(formula = Y ~ X1 + X2 + X3 + V_star + X2*X3,
                        v_star = "V_star", data = data)

summary(NB_mod)

# Coefficients:
#             Estimate Std. Error   t value Pr(>|t|)
# (Intercept)  0.89452    0.11135   8.03331   <2e-16 ***
# X1           2.13269    0.08473  25.17143   <2e-16 ***
# X2b         -0.01559    0.12545  -0.12429  0.90111
# X2c         -0.95827    0.11665  -8.21469   <2e-16 ***
# X3           4.09454    0.09061  45.19049   <2e-16 ***
# V_star       2.08187    0.08503  24.48402   <2e-16 ***
# X2b:X3       1.84705    0.13130  14.06693   <2e-16 ***
# X2c:X3      -2.11044    0.11910 -17.72024   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ---
#      AIC     AICc      BIC
# 7740.111 7740.334 7789.189
# ---
# Log-Likelihood
#      -3860.056
# ---
# Lambda:  0.08479587 std.err:  0.01901557


# Model fitting with a polynomial effect;

a8 <- -0.5

mu <- mu*exp(a8*X3^2)

Y <- rnbinom(n, size = 1, mu = mu)

data$Y <- Y

NB_mod <- nbRegMisrepEM(formula = Y ~ X1 + X2 + X3 + V_star + X2*X3 + I(X3^2),
                        v_star = "V_star", data = data)

summary(NB_mod)

# Coefficients:
#             Estimate Std. Error   t value Pr(>|t|)
# (Intercept)  0.96498    0.11201   8.61478   <2e-16 ***
# X1           2.09647    0.09310  22.51926   <2e-16 ***
# X2b         -0.02546    0.13341  -0.19082   0.8487
# X2c         -1.08524    0.12751  -8.51091   <2e-16 ***
# X3           4.03397    0.11939  33.78945   <2e-16 ***
# V_star       1.99765    0.09395  21.26217   <2e-16 ***
# I(X3^2)     -0.49023    0.05312  -9.22849   <2e-16 ***
# X2b:X3       2.00513    0.14127  14.19333   <2e-16 ***
# X2c:X3      -1.93432    0.13657 -14.16309   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ---
#      AIC     AICc      BIC
# 7181.267 7181.535 7235.253
# ---
# Log-Likelihood
#      -3579.634
# ---
# Lambda:  0.1039235 std.err:  0.02154315

}

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
