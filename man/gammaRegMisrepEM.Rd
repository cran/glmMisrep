\name{gammaRegMisrepEM}
\alias{gammaRegMisrepEM}
\title{Fit a Gamma Misrepresentation Model using EM Algorithm}
\description{
\code{gammaRegMisrepEM} is used to fit a Gamma regression model, adjusting for misrepresentation on a binary predictor. The function uses the Expectation Maximization algorithm and allows multiple additional correctly measured independent variables in the Gamma regression with a log-link function that is typically used in insurance claims modeling. Standard errors of model estimates are obtained from closed form expressions of the Observed Fisher Information.
}
\usage{
gammaRegMisrepEM(formula, v_star, data, lambda = c(0.6,0.4),
                 epsilon = 1e-08, maxit = 10000,
                 maxrestarts = 20, verb = FALSE)
}
\arguments{
\item{formula}{an object of class "\code{\link{formula}}" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.}

\item{v_star}{a character specifying the name of the binary predictor that is suspected of being misrepresented.}

\item{data}{a dataframe containing the variables in the model.}

\item{lambda}{initial mixing proportions used to start the EM algorithm. A numeric vector of length two, with the second element being the prevalence of misrepresentation.}

\item{epsilon}{tolerance for convergence. Convergence is reached when the log-likelihood increases by less than epsilon.}

\item{maxit}{the maximum number of iterations the EM routine will run for.}

\item{maxrestarts}{how many times the EM routine will attempt to converge. When conergence is not achieved, the EM routine restarts with new randomly selected mixing proportions.}

\item{verb}{logical. If TRUE, the difference in new .vs. old log-likelihood and the current log-likelihood is printed to the console after every iteration. If TRUE, the user will also be notifed if the EM algorithm must restart with new mixing proportions.}

}
\details{
Models for \code{gammaRegMisrepEM} are specified symbolically. Like the \code{lm} and \code{glm} functions, the model has the form \code{response ~ terms}, where \code{response} is the numeric response vector and \code{terms} is a series of terms which specifies a linear predictor for \code{response}.

Currently, formula specification can accommodate the following expressions:

\itemize{
\item{transformations of the response: \code{log(y) ~ x}}
\item{polynomial terms: \code{y ~ x + I(x^2)}}
\item{interactions: \code{y ~ x*z}}
}

Including an offset term (e.g. \code{y ~ x + offset()}) is currently not supported.

}
\value{
\code{gammaRegMisrepEM} returns an object of \code{\link{class}} \code{"misrepEM"}.

The function \code{summary} is used to obtain and print a summary of the results.

An object of class \code{"misrepEM"} is a list containing the following 14 elements:

\item{y}{the response used.}

\item{lambda}{numeric. The estimated prevalence of misrepresentation.}

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

phi <- 0.2
alpha0 <- 1/phi
beta <- 1/mu/phi
Y <- rgamma(n, alpha0, beta)

data <- data.frame(Y = Y, X1 = X1, X2 = X2, X3 = X3, V_star = V_star)

# "a" is the reference
data$X2 <- as.factor(data$X2)

# Model with main effects:
gamma_mod <- gammaRegMisrepEM(formula = Y ~ X1 + X2 + X3 + V_star,
                              v_star = "V_star", data = data)

# The prevalence of misrepresentation;
(theta0 * p0) / (1 - theta0*(1-p0)) # 0.09677419

# Parameter estimates and estimated prevalence of
# misrepresentation (lambda);
summary(gamma_mod)

# Coefficients:
#             Estimate Std. Error   t value Pr(>|t|)
# (Intercept)  0.99356    0.03013  32.97245   <2e-16 ***
# X1           2.02152    0.03078  65.68276   <2e-16 ***
# X2b         -0.00679    0.03708  -0.18309  0.85477
# X2c         -1.02578    0.03684 -27.84599   <2e-16 ***
# X3           3.97883    0.01495 266.21973   <2e-16 ***
# V_star       2.00437    0.03107  64.51234   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ---
#      AIC     AICc      BIC
# 5650.696 5650.841 5689.958
# ---
# Log-Likelihood
#      -2817.348
# ---
# Lambda:  0.1083894 std.err:  0.01160662


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

beta <- 1/mu/phi
Y <- rgamma(n, alpha0, beta)

data$Y <- Y

gamma_mod <- gammaRegMisrepEM(formula = Y ~ X1 + X2 + X3 + V_star + X2*X3,
                              v_star = "V_star", data = data)

summary(gamma_mod)

# Coefficients:
#             Estimate Std. Error   t value Pr(>|t|)
# (Intercept)  0.96205    0.03086  31.17145   <2e-16 ***
# X1           2.00411    0.03061  65.46734   <2e-16 ***
# X2b         -0.00987    0.03682  -0.26818  0.78862
# X2c         -0.99957    0.03733 -26.77449   <2e-16 ***
# X3           3.98282    0.02484 160.31083   <2e-16 ***
# V_star       2.01107    0.03077  65.36550   <2e-16 ***
# X2b:X3       1.95884    0.03573  54.82466   <2e-16 ***
# X2c:X3      -1.98595    0.03567 -55.67827   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ---
#      AIC     AICc      BIC
# 5633.984 5634.207 5683.062
# ---
# Log-Likelihood
#      -2806.992
# ---
# Lambda:  0.1131951 std.err:  0.01181678


# Model fitting with a polynomial effect;

a8 <- -0.5

mu <- mu*exp(a8*X3^2)

beta <- 1/mu/phi
Y <- rgamma(n, alpha0, beta)

data$Y <- Y

gamma_mod <- gammaRegMisrepEM(formula = Y ~ X1 + X2 + X3 + V_star + X2*X3 + I(X3^2),
                              v_star = "V_star", data = data)

summary(gamma_mod)

# Coefficients:
#             Estimate Std. Error   t value Pr(>|t|)
# (Intercept)  1.04312    0.03164  32.96624   <2e-16 ***
# X1           2.04411    0.02929  69.79020   <2e-16 ***
# X2b         -0.10418    0.03512  -2.96620  0.00309  **
# X2c         -1.08910    0.03531 -30.84683   <2e-16 ***
# X3           4.00265    0.02421 165.31001   <2e-16 ***
# V_star       1.98741    0.02951  67.35719   <2e-16 ***
# I(X3^2)     -0.51152    0.01350 -37.90112   <2e-16 ***
# X2b:X3       1.98709    0.03598  55.22750   <2e-16 ***
# X2c:X3      -2.03395    0.03692 -55.09491   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ---
#      AIC     AICc      BIC
# 4559.969 4560.236 4613.954
# ---
# Log-Likelihood
#      -2268.984
# ---
# Lambda:  0.111464 std.err:  0.01173143

}

}
\references{
Xia, Michelle, Rexford Akakpo, and Matthew Albaugh. "Maximum Likelihood Approaches to Misrepresentation Models in GLM ratemaking: Model Comparisons." \emph{Variance} 16.1 (2023).

Akakpo, R. M., Xia, M., & Polansky, A. M. (2019). Frequentist inference in insurance ratemaking models adjusting for misrepresentation. \emph{ASTIN Bulletin: The Journal of the IAA, 49}(1), 117-146.

Xia, M., Hua, L., & Vadnais, G. (2018). Embedded predictive analysis of misrepresentation risk in GLM ratemaking models. \emph{Variance, 12}(1), 39-58.

}
