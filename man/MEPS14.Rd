\name{MEPS14}
\alias{MEPS14}
\docType{data}
\title{
MEPS 2014 Full Year Consolidated Data File
}
\description{
MEPS14 is a subset of the MEPS 2014 Full Year Consolidated Data File, as described in Xia et. al., (2023).
}
\usage{data("MEPS14")}
\format{
  A data frame with 13,301 observations on the following 7 variables:
  \describe{
    \item{\code{TOTEXP14}}{total medical expenditure.}
    \item{\code{OBTOTV14}}{total number of office-based visits.}
    \item{\code{UNINS14}}{uninsured status (1 - insured, 0 - uninsured).}
    \item{\code{SEX}}{sex (1 - male, 0 - female).}
    \item{\code{AGE14X}}{age.}
    \item{\code{ADSMOK42}}{smoking status (1 - yes, 0 - no).}
    \item{\code{RTHLTH53}}{perceieved health status (1 - excellent, 5 - poor).}
  }
}

\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
\url{https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_detail.jsp?cboPufNumber=HC-171}
}
\references{
Xia, Michelle, Rexford Akakpo, and Matthew Albaugh. "Maximum Likelihood Approaches to Misrepresentation Models in GLM ratemaking: Model Comparisons." \emph{Variance} 16.1 (2023).
}
\examples{
\donttest{

# Reproducing table 4 in Xia et. al., (2023).

data(MEPS14)

colMeans(MEPS14)
#    TOTEXP14     OBTOTV14      UNINS14          SEX       AGE14X     ADSMOK42     RTHLTH53
#5042.4647771    6.2260732    0.1242012    0.4153071   41.6628825    0.1670551    2.4319224

apply(MEPS14, 2, sd)
#    TOTEXP14     OBTOTV14      UNINS14          SEX       AGE14X     ADSMOK42     RTHLTH53
#1.358567e+04 1.272065e+01 3.298233e-01 4.927934e-01 1.332746e+01 3.730391e-01 1.074713e+00

sum(MEPS14$OBTOTV14 == 0 ) / nrow(MEPS14)
# [1] 0.1595369

sd(MEPS14$OBTOTV14 == 0)
# [1] 0.3661898

# Fit Gamma regression model with insured status as
# the misrepresented variable.
MEPS14$RTHLTH53 <- as.factor(MEPS14$RTHLTH53)

gamma_fit <- gammaRegMisrepEM(formula = TOTEXP14 ~ UNINS14
             + SEX + AGE14X + ADSMOK42 + RTHLTH53,
             v_star = "UNINS14", data = MEPS14)

# summary returns a table of summary statistics, including
# goodness of fits (AIC, AICc, BIC), as well as the
# estimated prevalence of misrepresentation.
summary(gamma_fit)

# Coefficients:
#             Estimate Std. Error   t value Pr(>|t|)
# (Intercept)  8.03379    0.05341 150.41937   <2e-16 ***
# UNINS14     -1.98132    0.03170 -62.49292   <2e-16 ***
# SEX         -0.20427    0.02669  -7.65320   <2e-16 ***
# AGE14X       0.02764    0.00099  27.83485   <2e-16 ***
# ADSMOK42    -0.08868    0.03653  -2.42776  0.01521   *
# RTHLTH532    0.24923    0.03533   7.05469   <2e-16 ***
# RTHLTH533    0.53860    0.03655  14.73488   <2e-16 ***
# RTHLTH534    1.00615    0.04837  20.80026   <2e-16 ***
# RTHLTH535    1.87845    0.08104  23.17833   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ---
#      AIC     AICc      BIC
# 241083.9 241083.9 241166.3
# ---
# Log-Likelihood
#      -120530.9
# ---
# Lambda:  0.7734337 std.err:  0.009628053


# Fit Lognormal regression model with insured status as
# the misrepresented variable.
LN_fit <- LnRegMisrepEM(formula = log(TOTEXP14) ~ UNINS14
             + SEX + AGE14X + ADSMOK42 + RTHLTH53,
             v_star = "UNINS14", data = MEPS14)

summary(LN_fit)

# Coefficients:
#             Estimate Std. Error   t value Pr(>|t|)
# (Intercept)  7.28974    0.05648 129.05986   <2e-16 ***
# UNINS14     -1.29503    0.05496 -23.56317   <2e-16 ***
# SEX         -0.29590    0.02808 -10.53844   <2e-16 ***
# AGE14X       0.02460    0.00107  23.10180   <2e-16 ***
# ADSMOK42    -0.07008    0.03756  -1.86591  0.06208   .
# RTHLTH532    0.26349    0.03831   6.87786   <2e-16 ***
# RTHLTH533    0.47184    0.03942  11.97017   <2e-16 ***
# RTHLTH534    1.05065    0.04990  21.05580   <2e-16 ***
# RTHLTH535    1.94978    0.08067  24.16987   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ---
#      AIC     AICc      BIC
# 239726.4 239726.4 239808.8
# ---
# Log-Likelihood
#      -119852.2
# ---
# Lambda:  0.1110631 std.err:  0.02548188


# Fit Negative Binomial regression model with insured status as
# the misrepresented variable.
NB_fit <- nbRegMisrepEM(formula = OBTOTV14 ~ UNINS14
             + SEX + AGE14X + ADSMOK42 + RTHLTH53,
             v_star = "UNINS14", data = MEPS14)


summary(NB_fit)

# Coefficients:
#             Estimate Std. Error   t value Pr(>|t|)
# (Intercept)  2.00472    0.05463  36.69491   <2e-16 ***
# UNINS14     -1.68638    0.03371 -50.02640   <2e-16 ***
# SEX         -0.40917    0.02303 -17.76536   <2e-16 ***
# AGE14X       0.01897    0.00087  21.91823   <2e-16 ***
# ADSMOK42    -0.11391    0.03038  -3.74948  0.00018 ***
# RTHLTH532    0.20720    0.03183   6.50966   <2e-16 ***
# RTHLTH533    0.36794    0.03240  11.35678   <2e-16 ***
# RTHLTH534    0.72357    0.03978  18.18859   <2e-16 ***
# RTHLTH535    1.24468    0.06281  19.81714   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ---
#      AIC     AICc      BIC
# 72788.71 72788.73 72871.16
# ---
# Log-Likelihood
#      -36383.35
# ---
# Lambda:  0.8351591 std.err:  0.009627158


# Fit Poisson regression model with smoking status as
# the misrepresented variable.
pois_fit <- poisRegMisrepEM(formula = OBTOTV14 ~ UNINS14
             + SEX + AGE14X + ADSMOK42 + RTHLTH53,
             v_star = "UNINS14", data = MEPS14)

summary(pois_fit)

# Coefficients:
#             Estimate Std. Error    z value Pr(>|z|)
# (Intercept)  2.27367    0.02276   99.87676   <2e-16 ***
# UNINS14     -2.03719    0.00730 -279.00809   <2e-16 ***
# SEX         -0.18594    0.01090  -17.05204   <2e-16 ***
# AGE14X       0.01631    0.00042   38.90467   <2e-16 ***
# ADSMOK42     0.09594    0.01313    7.30930   <2e-16 ***
# RTHLTH532    0.14918    0.01641    9.09033   <2e-16 ***
# RTHLTH533    0.31282    0.01620   19.31078   <2e-16 ***
# RTHLTH534    0.75044    0.01793   41.85270   <2e-16 ***
# RTHLTH535    1.09859    0.02265   48.49410   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ---
#      AIC     AICc      BIC
# 99599.31 99599.33 99674.27
# ---
# Log-Likelihood
#      -49789.66
# ---
# Lambda:  0.85957 std.err:  0.00348128

}
}
\keyword{datasets}
