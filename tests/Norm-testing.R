require(glmMisrep)

data <- data.frame(Y = c(5.7773377,  4.0771101, 10.6148439,  4.0595742,  6.8090699,  5.6104512,  1.5086211,  3.4320585,  3.9744215,
                         2.8556719,  4.4818115,  7.9074660,  6.4712724,  5.2131165,  7.0006130,  5.5061077,  0.9458157,  5.4395321,
                         7.4082901,  8.2189500,  5.8960478,  2.8024485,  6.8348760,  2.7883808,  6.0570349,  6.7937266,  3.9282244,
                         7.4247408,  8.2522322,  4.5424937,  5.0515150,  2.5093128,  4.8906243,  2.5215893,  4.3976078,  4.3180865,
                         8.2802522,  7.7317326,  2.4434248,  2.0071280,  3.5337221,  0.7831891,  7.8267108,  4.6097870,  3.5170801,
                         4.3757977,  5.1311735,  4.2048721,  6.1684635,  4.9051138),
                   X1 = c(0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0,
                          1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1),
                   X2 = c(-0.76197819,  0.14641157,  0.33842474, -0.84064339, -0.38139435, -0.83670237, -0.93504912,  0.31521823,
                          1.18613960, -2.35301938,  0.60325140, -1.41359783,  0.21377037, -2.25124434, -0.13338382, -1.78561439,
                          -0.90558730,  0.67648677,  0.69648471,  0.23289791,  0.12483888,  0.29052792, -0.71775455, -0.72374960,
                          -0.83533896, -0.49602035, -2.48825650, -1.02971441, -1.50495109,  0.16223104, -1.43348993,  1.31988160,
                          -0.28163285,  0.43728953, -0.38357623,  1.78988984,  1.07001608, -0.26095291,  0.36204512, -0.03076897,
                          0.31181992,  0.38221827, -1.26748568, -1.44279044,  0.37183778,  0.29513165,  1.21310991,  0.88710040,
                          0.22622264,  0.16506846),
                   X3 = c(0.7998339, 0.6163772, 0.9965978, 0.6586332, 0.3164639, 0.9467281, 0.1978913, 0.4730891, 0.6139086, 0.2917088,
                          0.8307349, 0.9226486, 0.6882567, 0.6316347, 0.8414099, 0.9763476, 0.0664963, 0.4005814, 0.7706873, 0.7736968,
                          0.8110066, 0.1558532, 0.9740569, 0.5948224, 0.6471430, 0.8082560, 0.8391456, 0.1788990, 0.8712758, 0.8122476,
                          0.3271224, 0.5994316, 0.9058301, 0.4301065, 0.1428866, 0.8295565, 0.8104529, 0.8057883, 0.2776331, 0.2635620,
                          0.5363262, 0.4056384, 0.4436944, 0.6197825, 0.9951726, 0.2532455, 0.5785129, 0.6998782, 0.9911132, 0.7314322),
                   Sex = c("Female", "Female", "Male",   "Female", "Male",   "Female", "Male",   "Female", "Male",   "Female", "Female",
                           "Female", "Male",   "Male",   "Male",   "Male",   "Female", "Male",   "Male",   "Female", "Female", "Female",
                           "Male",   "Female", "Male",   "Male",   "Male",   "Male",   "Female", "Female", "Male",   "Female", "Female",
                           "Female", "Female", "Female", "Female", "Male",   "Female", "Female", "Male",   "Female", "Male",   "Male",
                           "Female", "Male",   "Female", "Female", "Male",   "Female"),
                   Race = c("Black", "Black", "White", "Other", "White", "Other", "Other", "Other", "Black", "White", "White", "Other",
                            "White", "White", "Other", "Black", "Black", "Black", "Black", "White", "Black", "Other", "White", "White",
                            "Other", "Black", "Other", "White", "White", "Black", "Black", "Other", "Black", "White", "Other", "Black",
                            "Black", "Other", "Black", "White", "White", "Other", "Black", "Black", "Other", "Other", "Black", "White",
                            "White", "Other"),
                   V_star = c(1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1,
                              0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0))

data$Race <- as.factor(data$Race)
data$Sex <- as.factor(data$Sex)

t1 <- tryCatch(NormRegMisrepEM(formula = y ~ X1 + X2 + X3 + Sex + Race + V_star,
                                v_star = "V_star",
                                data = data,
                                lambda = c(0.6,0.4),
                                epsilon = 1e-08,
                                maxit = 10000,
                                maxrestarts = 20),
               error = function(x) x )

# The response above is inappropriately specified (should be Y, not y)
stopifnot(
  t1$message == "object 'y' not found"
)


t2 <- tryCatch(NormRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
                                v_star = "V_Star",
                                data = data,
                                lambda = c(0.6,0.4),
                                epsilon = 1e-08,
                                maxit = 10000,
                                maxrestarts = 20),
               error = function(x) x )

# Argument to 'v_star' is misspelled
stopifnot(
  t2$message == "variable V_Star not present in dataframe"
)


data$V_star <- ifelse(data$V_star == 1, yes = "yes", no = "no")

t3 <- tryCatch(NormRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
                                v_star = "V_star",
                                data = data,
                                lambda = c(0.6,0.4),
                                epsilon = 1e-08,
                                maxit = 10000,
                                maxrestarts = 20),
               error = function(x) x )


# v* variable is type character (yes and no)
stopifnot(
  t3$message == "v_star variable must be of class 'factor' or 'numeric'"
)


data$V_star <- ifelse(data$V_star == "yes", yes = 1, no = 0)


data$V_star[10] <- -1


t4 <- tryCatch(NormRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
                                v_star = "V_star",
                                data = data,
                                lambda = c(0.6,0.4),
                                epsilon = 1e-08,
                                maxit = 10000,
                                maxrestarts = 20),
               error = function(x) x )

# v* variable must be binary
stopifnot(
  t4$message == "v_star variable must contain two unique values"
)

data$V_star[10] <- 0

data$V_star <- ifelse(data$V_star == 1, yes = 1, no = 2)

t5 <- tryCatch(NormRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
                                v_star = "V_star",
                                data = data,
                                lambda = c(0.6,0.4),
                                epsilon = 1e-08,
                                maxit = 10000,
                                maxrestarts = 20),
               error = function(x) x )

# v* must be binary, but more specifically 0/1;
stopifnot(
  t5$message == "v_star variable must be coded with ones and zeroes"
)


data$V_star <- ifelse(data$V_star == 1, yes = 1, no = 0)

t6 <- tryCatch(NormRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
                                v_star = "V_star",
                                data = data,
                                lambda = c(0.49, 0.52),
                                epsilon = 1e-08,
                                maxit = 10000,
                                maxrestarts = 20),
               error = function(x) x )

# Inappropriately specified lambda argument
stopifnot(
  t6$message == "Lambda vector must sum to one"
)


t7 <- tryCatch(NormRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
                                v_star = "V_star",
                                data = data,
                                lambda = c(1/3, 1/3, 1/3),
                                epsilon = 1e-08,
                                maxit = 10000,
                                maxrestarts = 20),
               error = function(x) x )

# Inappropriately specified lambda argument
stopifnot(
  t7$message == "Lambda vector must contain two elements"
)

data$X4 <- data$X2*0.3

t8 <- tryCatch(NormRegMisrepEM(formula = Y ~ X1 + X2 + X3 + X4 + Sex + Race + V_star,
                                v_star = "V_star",
                                data = data,
                                lambda = c(0.6, 0.4),
                                epsilon = 1e-08,
                                maxit = 10000,
                                maxrestarts = 20),
               error = function(x) x )

# Linearly dependent covariates/degenerate design matrix
stopifnot(
  t8$message == "Linear dependencies exist in the covariates"
)


t9 <- tryCatch(NormRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race,
                                v_star = "V_star",
                                data = data,
                                lambda = c(0.6, 0.4),
                                epsilon = 1e-08,
                                maxit = 10000,
                                maxrestarts = 20),
               error = function(x) x )

# V_star variable is not present in formula argument
stopifnot(
  t9$message == "v_star variable must be specified in 'formula'"
)



# EM algorithm should fail to converge within the specified number of attempts
t10 <- tryCatch(
  capture.output(NormRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
                                  v_star = "V_star",
                                  data = data,
                                  lambda = c(0.6, 0.4),
                                  epsilon = 1e-08,
                                  maxit = 2,
                                  maxrestarts = 1)),
  error = function(x) x
)

stopifnot(
  t10$message == "NOT CONVERGENT! Failed to converge after 1 attempts"
)


# On the first attempt, fails to converge, and restarts with new mixing props.
# Succeeds on the second attempt.
msg <- capture.output(
  t11 <-  NormRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
                           v_star = "V_star",
                           data = data,
                           lambda = c(0.6, 0.4),
                           epsilon = 1e-08,
                           maxit = 6,
                           maxrestarts = 4, verb = "TRUE"),
  type = "message"
)

stopifnot(
  any(msg == "Warning: Failed to converge. Restarting with new mixing proportions")
)



# This should succeed;
msg <- capture.output(
  t12 <-  NormRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
                           v_star = "V_star",
                           data = data,
                           lambda = c(0.6, 0.4),
                           epsilon = 1e-08,
                           maxit = 10000,
                           maxrestarts = 20),
  type = "message"
)

# Output validation;

# Outpute should be a list
stopifnot(
  is.list(t12)
)

# With 14 elements
stopifnot(
  length(t12) == 14
)

# Fisher information matrix should be symmetric
stopifnot(
  isSymmetric(t12$cov.estimates)
)


# The returned list should have elements with the following names
# and types
stopifnot(
  all.equal(lapply(t12, class),
            lapply(list(y = 0.1, lambda = 0.2, params = 0.3, loglik = 0.4,
                        posterior = 0.5, all.loglik = 0.6, cov.estimates = matrix(data = c(1,2,3,4), 2, 2),
                        std.error = 0.7, t.values = 0.8, p.values = 0.9, ICs = 1.0, ft = "*",
                        formula = y ~ x, v_star_name = "v*" ), class) )
)


# Verifying the function can correctly calculate things;
stopifnot(
  all.equal(t12$lambda, 0.1023287, tolerance = 2e-7 )
)

stopifnot(
  all.equal(as.numeric(t12$params), c(4.117078e-01,  1.017801e+00,  1.893753e+00, -5.348563e-05,  3.886515e+00,  1.011545e+00, -1.084811e+00,
                                      5.105842e-02,  2.167551e+00), tolerance = 2e-7 )
)

stopifnot(
  all.equal( t12$loglik, -39.374, tolerance = 2e-7)
)

stopifnot(
  all.equal( t12$posterior, c(6.176011e-03, 9.994703e-01, 3.916967e-09, 1.000000e+00, 1.106437e-02, 9.999996e-01, 1.000000e+00,
                              5.474239e-03, 1.000000e+00, 9.995386e-01, 9.999988e-01, 1.279787e-07, 1.000000e+00, 9.993667e-01,
                              3.091474e-09, 1.000000e+00, 1.000000e+00, 9.999999e-01, 9.999426e-01, 2.950874e-06, 1.000000e+00,
                              9.999876e-01, 1.000000e+00, 1.000000e+00, 3.447759e-08, 1.000000e+00, 1.000000e+00, 4.576997e-09,
                              2.464170e-04, 9.999879e-01, 1.000000e+00, 9.999974e-01, 9.999901e-01, 1.000000e+00, 5.870045e-05,
                              9.999997e-01, 4.355558e-06, 1.504731e-03, 9.999908e-01, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                              7.245545e-06, 9.999990e-01, 1.000000e+00, 9.998710e-01, 4.019383e-04, 9.999777e-01, 9.999978e-01,
                              9.999978e-01), tolerance = 2e-7)
)


stopifnot(
  all.equal( t12$all.loglik, c(-62.07717, -43.03354, -39.76408, -39.38987, -39.37410, -39.37400, -39.37400, -39.37400), tolerance = 2e-7  )
)


stopifnot(
  all.equal(t12$cov.estimates,
            matrix(data = c(2.364235e-03, -1.418045e-05, -2.372655e-05, -2.493742e-05, -1.954939e-06,  6.354047e-05, -2.330355e-05,  3.394952e-06, -4.072659e-05,  5.941786e-06,
                            -1.418045e-05,  1.734607e-03,  3.259051e-05,  6.994462e-05,  1.297886e-05, -1.235953e-04,  5.330036e-05,  2.627034e-06,  8.365114e-05, -3.474248e-05,
                            -2.372655e-05,  3.259051e-05,  3.319225e-02, -3.691302e-03, -2.362637e-04, -3.098723e-02, -5.167991e-03, -1.034864e-02, -9.042696e-03, -1.356344e-03,
                            -2.493742e-05,  6.994462e-05, -3.691302e-03,  1.586459e-02, -1.082851e-04, -1.925058e-04, -2.871320e-03, -1.240225e-03, -6.301564e-05, -4.909270e-03,
                            -1.954939e-06,  1.297886e-05, -2.362637e-04, -1.082851e-04,  4.010482e-03, -8.215237e-04,  2.121940e-03,  1.121342e-03,  1.089475e-03,  3.152799e-04,
                             6.354047e-05, -1.235953e-04, -3.098723e-02, -1.925058e-04, -8.215237e-04,  4.990288e-02, -1.032964e-03,  1.752904e-03,  2.422815e-06, -1.426289e-03,
                            -2.330355e-05,  5.330036e-05, -5.167991e-03, -2.871320e-03,  2.121940e-03, -1.032964e-03,  1.566136e-02,  1.863244e-03,  4.191628e-06,  4.047063e-04,
                             3.394952e-06,  2.627034e-06, -1.034864e-02, -1.240225e-03,  1.121342e-03,  1.752904e-03,  1.863244e-03,  2.097566e-02,  9.925793e-03, -2.168029e-03,
                            -4.072659e-05,  8.365114e-05, -9.042696e-03, -6.301564e-05,  1.089475e-03,  2.422815e-06,  4.191628e-06,  9.925793e-03,  2.070577e-02, -1.373348e-03,
                             5.941786e-06, -3.474248e-05, -1.356344e-03, -4.909270e-03,  3.152799e-04, -1.426289e-03,  4.047063e-04, -2.168029e-03, -1.373348e-03,  1.809059e-02),
                   ncol = 10, nrow = 10, byrow = TRUE, dimnames = list( c("lambda", names(t12$params)), c("lambda", names(t12$params)) ) ),
             tolerance = 2e-7
  )
)



stopifnot(
  all.equal(as.numeric(t12$std.error), c(0.04862340, 0.04164862, 0.18218741, 0.12595470, 0.06332837, 0.22338952, 0.12514534, 0.14482975, 0.14389499, 0.13450125), tolerance = 2e-7)
)

stopifnot(
  all.equal(as.numeric(t12$t.values), c(5.5865629427, 15.0351923749, -0.0008445761, 17.3979285916,  8.0829605059, -7.4902475756,  0.3548311040, 16.1154689801), tolerance = 2e-7)
)


stopifnot(
  all.equal(as.numeric(t12$p.values), c(1.800248e-06, 4.714854e-18, 9.993303e-01, 2.930997e-20, 6.103688e-10, 3.929716e-09, 7.245802e-01, 4.314661e-19), tolerance = 2e-7)
)

stopifnot(
  all.equal(as.numeric(t12$ICs), c(98.74799, 104.38902, 117.86822), tolerance = 2e-7)
)


stopifnot(
  t12$ft == "NormRegMisrepEM"
)

stopifnot(
  class(t12$formula) == "formula"
)

stopifnot(
  t12$v_star_name == "V_star"
)


# Test S3 method for summarizing misrepEM objects;
stopifnot(
  class(summary(t12)) == "summary.misrepEM"
)

# Output needs to be a list
stopifnot(
  is.list(summary(t12))
)

# of length 5
stopifnot(
  length(summary(t12)) == 5
)

# Whose elements are: (1) dataframe, and (2-5) 4 numeric vectors, which have the following names:
stopifnot(
  all.equal(lapply(summary(t12), FUN = class), list(coefficients = "data.frame",
                                                    ICs = "numeric",
                                                    loglik = "numeric",
                                                    lambda = "numeric",
                                                    lambda_stderror = "numeric") )
)


# Test S3 predict method
test_data <- data.frame(Y = c(8.6152613, 5.5227015, 7.1853026, 3.0298131, 6.6578348, 7.5547255, 4.0512950, 0.6793011, 7.0572102, 7.6823568, 6.6596103, 7.3518760,
                              5.3870262, 2.5072679, 2.5140229, 5.4396757, 7.6588728, 3.7428609, 7.9495086, 3.8790915, 9.7152705, 4.4887421, 1.5031392, 4.8726583,
                              9.0975810, 4.8005682, 5.3351505, 3.6656452, 5.5514505, 5.9420986, 5.6341074, 5.9983096, 4.8619339, 3.5179743, 6.2989394, 2.0622352,
                              3.1392840, 6.8755296, 5.7640324, 4.5445463, 4.0678373, 2.3591591, 4.7524989, 5.4248010, 5.6977598, 6.7629305, 5.4208523, 5.1906456,
                              5.0067083, 2.6607524),
                        X1 = c(1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1,
                               1, 0, 0, 1, 0, 0, 0),
                        X2 = c(0.73920240, -1.13866423,  0.24403059, -0.39677036,  1.20298698,  0.10839447, -0.03864269, -0.53201547,  1.08369674,  1.42525375,
                               -0.53386477,  0.66247740, -1.48153510, -0.10252219, -0.30548863, -0.38257197,  0.73434161, -1.64458422, -1.56723525,  0.14294176,
                                0.30668455, -0.80253947,  1.43043993,  0.40769766, -0.08469838,  0.35901292, -1.03301052,  0.83536897, -0.13129073, -1.17009726,
                               -1.70216401, -0.97153429, -0.26962895,  0.54355290, -0.11689035, -0.57516781,  0.93324617,  1.58045873,  1.55271413, -1.42611936,
                                0.19323935,  0.03823235, -0.29322422,  0.57581869,  0.54809371, -0.11096769, -0.55482619,  1.10868074, -0.91262892, -0.16269909),
                        X3 = c(0.75441408, 0.40624903, 0.75354202, 0.31822297, 0.32844024, 0.98879637, 0.62617695, 0.37586682, 0.94915067, 0.99189404, 0.91812200,
                               0.96812078, 0.58573640, 0.73676152, 0.34820258, 0.23719973, 0.88541023, 0.66537580, 0.99520899, 0.80875078, 0.87426843, 0.82742784,
                               0.32371491, 0.65195113, 0.76574375, 0.49912156, 0.35071586, 0.18645295, 0.70230509, 0.90138070, 0.89432553, 0.94434558, 0.98693010,
                               0.99500845, 0.51159103, 0.52419973, 0.62819345, 0.94240240, 0.96089396, 0.65406248, 0.77223549, 0.71330369, 0.09700493, 0.82867002,
                               0.92117975, 0.93079926, 0.32226879, 0.92469635, 0.87559209, 0.29989646),
                        Sex = c("Male",   "Male",   "Male",   "Male",   "Female", "Male",   "Male",   "Female", "Male",   "Male",   "Female", "Female", "Male",
                                "Female", "Male",   "Male",   "Female", "Female", "Male",   "Male",   "Male",   "Male",   "Female", "Male",   "Male",   "Male",
                                "Male",   "Male",   "Female", "Male",   "Male",   "Female", "Female", "Female", "Male",   "Female", "Female", "Male",   "Male",
                                "Male",   "Female", "Female", "Male",   "Female", "Male",   "Male",   "Male",   "Male",   "Male",   "Female"),
                        Race = c("Other", "Black", "White", "Black", "Other", "Black", "Other", "Other", "Other", "White", "White", "White", "Other", "Other", "Other",
                                 "Black", "Other", "Black", "White", "Other", "Black", "Other", "White", "Other", "Black", "Other", "Black", "Black", "White", "Black",
                                 "Black", "Other", "White", "Other", "Black", "Other", "Black", "Other", "Other", "Black", "Black", "Other", "Black", "Black", "White",
                                 "Black", "White", "Black", "White", "Black"),
                        V_star = c(0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 1, 0, 0, 0, 0))

# Output needs to be a numeric vector
stopifnot(
  is.vector(predict(t12, test_data)) && is.numeric(predict(t12, test_data))
)


stopifnot(
  all.equal(predict(t12, test_data), c(5.992093, 5.723856, 7.176595, 3.487948, 5.270718, 8.039863, 3.599986, 1.615634, 6.748922, 8.102890, 6.804734, 6.946996, 5.336644,
                                       3.018233, 2.519649, 5.066803, 7.435415, 3.825685, 8.063939, 4.309552, 7.542743, 4.382192, 2.548709, 3.700134, 9.066729, 4.999916,
                                       5.508020, 4.921504, 5.965937, 5.754441, 5.727049, 5.770807, 5.126395, 4.021879, 6.133215, 2.192134, 3.681037, 6.722668, 4.900784,
                                       4.793249, 4.240898, 2.927057, 4.521929, 6.353965, 5.882357, 7.814468, 5.448493, 5.844936, 5.705258, 2.405165), tolerance = 2e-7)
)

