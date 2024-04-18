require(glmMisrep)

data <- data.frame( Y = c(0,  0,  0,  0,  0,  5,  1,  0,  0,  0,  4,  2,  3,  3, 29,  0,  0, 12,  0,  6,  0,  0,  0,  1,  3, 17,  0, 25,  0,  0,  2,  0,  0,  0,  0, 0,  0,
                          4,  2, 16,  0, 19,  0,  0,  2,  0,  0,  0,  0,  0),
                    X1 = c(0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0,
                           1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0),
                    X2 = c(0.315223940,  0.203016819, -0.046615492, -0.040681413,  0.617273624,  0.813577139,  0.434980275, -1.868117146,
                            -0.027052575,  0.228288655,  1.380955283,  0.257543733, -0.353957121,  0.818737902,  0.525026487, -0.506164644,
                            -0.949480806, -0.107608844,  0.363661818, -0.295835692,  0.110056356, -0.205246487, -0.285092978, -0.639260383,
                             1.576928404,  2.058907651,  0.823073891, -0.340826703, -1.324805151,  0.103545160,  0.497766445,  0.501280218,
                            -0.297921410,  1.056264736, -0.767076814, -0.212290592, -1.563216428,  1.026954673, -0.087597819,  1.334059105,
                             0.955057522,  1.823380042,  1.082772308, -2.223962848,  1.433183408, -0.004531971, -1.029210193,  0.532295431,
                            -0.930908683,  0.391930241),
                    X3 = c(0.74306067, 0.67595394, 0.96320251, 0.57668303, 0.81643636, 0.78038200, 0.49999395, 0.99676766, 0.97642715, 0.88478779,
                            0.53447733, 0.27009525, 0.64544670, 0.63666898, 0.14489153, 0.89359811, 0.70506987, 0.81595152, 0.78411313, 0.90301090,
                            0.48920726, 0.75707194, 0.98999417, 0.85091229, 0.29074286, 0.73562357, 0.97903729, 0.96906234, 0.84957226, 0.55937779,
                            0.49149558, 0.83789430, 0.66902416, 0.44173571, 0.81911265, 0.64182433, 0.34363554, 0.78838686, 0.78557154, 0.83241653,
                            0.68691255, 0.20317013, 0.78619988, 0.09229911, 0.52779899, 0.79314940, 0.95612951, 0.95234203, 0.54259470, 0.81656990),
                    Sex = c("Male",   "Male",   "Female", "Male",   "Male",   "Female", "Female", "Female", "Female", "Male",   "Male",   "Female",
                             "Male",   "Female", "Male",   "Female", "Female", "Male",   "Female", "Male",   "Male",   "Female", "Male",   "Female",
                             "Female", "Male",   "Male",   "Male",   "Male",   "Female", "Male",   "Female", "Female", "Female", "Male",   "Female",
                             "Male",   "Female", "Male",   "Female", "Female", "Male",   "Female", "Male",   "Male",   "Female", "Female", "Female",
                             "Female", "Male"),
                    Race = c("Black", "Black", "White", "White", "White", "White", "Black", "Other", "Other", "Other", "Black", "Other", "Other", "Other", "Other", "Other", "White", "Other",
                              "White", "Black", "White", "Other", "White", "White", "Other", "Other", "Black", "Other", "Other", "Black", "Black", "Black", "Other", "Black", "Black", "Other",
                              "White", "Other", "White", "White", "Other", "Other", "Other", "White", "White", "Black", "Black", "Black", "Other", "White"),
                    V_star = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

data$Race <- as.factor(data$Race)
data$Sex <- as.factor(data$Sex)

t1 <- tryCatch(nbRegMisrepEM(formula = y ~ X1 + X2 + X3 + Sex + Race + V_star,
                                v_star = "V_star",
                                data = data,
                                lambda = c(0.6,0.4),
                                epsilon = 1e-08,
                                maxit = 10000,
                                maxrestarts = 20),
               error = function(x) x )

# The response above is inappropriately specified (y, not Y)
stopifnot(
  t1$message == "object 'y' not found"
)


t2 <- tryCatch(nbRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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

t3 <- tryCatch(nbRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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


t4 <- tryCatch(nbRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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

t5 <- tryCatch(nbRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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

t6 <- tryCatch(nbRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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


t7 <- tryCatch(nbRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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

t8 <- tryCatch(nbRegMisrepEM(formula = Y ~ X1 + X2 + X3 + X4 + Sex + Race + V_star,
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

# This is only to make sure the glm.nb() function can fit a model
# without throwing warnings messages/failing to converge. For purposes
# of testing our error handling, this should work.

data$VS <- data$V_star

t9 <- tryCatch(nbRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + VS,
                             v_star = "V_star",
                             data = data,
                             lambda = c(0.6, 0.4),
                             epsilon = 1e-08,
                             maxit = 10000,
                             maxrestarts = 20),
               error = function(x) x )

# V_star variable is absent from formula ragument
stopifnot(
  t9$message == "v_star variable must be specified in 'formula'"
)



# EM algorithm should fail to converge within the specified number of attempts
t10 <- tryCatch(
  capture.output(nbRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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
  t11 <-  nbRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
                           v_star = "V_star",
                           data = data,
                           lambda = c(0.6, 0.4),
                           epsilon = 1e-08,
                           maxit = 9,
                           maxrestarts = 4, verb = TRUE),
  type = "message"
)

stopifnot(
  any(msg == "Warning: Failed to converge. Restarting with new mixing proportions")
)



msg <- capture.output(
 t12 <-  nbRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
                                v_star = "V_star",
                                data = data,
                                lambda = c(0.6, 0.4),
                                epsilon = 1e-08,
                                maxit = 10000,
                                maxrestarts = 20),
 type = "message"
  )

# Output validation;

# Output should be a list
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
  all.equal(t12$lambda, 0.08400328, tolerance = 3e-7 )
)

stopifnot(
  all.equal(as.numeric(t12$params), c(6.7430448, -2.9549985,  0.9375115,  1.7238887, -0.2716501,  1.1538932,  1.0963831, -0.5272682,  3.3533715), tolerance = 3e-7 )
)

stopifnot(
  all.equal( t12$loglik, -65.62407, tolerance = 3e-7 )
)

stopifnot(
  all.equal( t12$posterior, c(9.988710e-01, 9.980942e-01, 9.509450e-01, 9.856863e-01, 9.987743e-01, 3.159226e-05, 9.864484e-01, 9.379181e-01,
                              9.928805e-01, 9.999618e-01, 9.999997e-01, 7.803974e-01, 1.890117e-01, 9.592257e-01, 1.196146e-16, 9.749377e-01,
                              9.258057e-01, 1.920442e-10, 9.722526e-01, 3.270529e-05, 9.992455e-01, 9.885919e-01, 9.931605e-01, 4.166431e-01,
                              9.999954e-01, 1.000000e+00, 9.999427e-01, 9.326991e-21, 9.678755e-01, 9.952032e-01, 9.997068e-01, 9.904702e-01,
                              9.981918e-01, 9.994231e-01, 9.887824e-01, 9.988644e-01, 9.277404e-01, 9.999515e-01, 9.990492e-02, 4.403466e-11,
                              9.999803e-01, 9.999980e-01, 9.999999e-01, 9.202387e-01, 9.999995e-01, 9.690717e-01, 9.291949e-01, 9.991821e-01,
                              9.566443e-01, 9.963779e-01), tolerance = 3e-7 )
)


stopifnot(
  all.equal( t12$all.loglik, c(-81.19736, -70.48727, -67.79435, -66.44099, -65.98469, -65.74817, -65.64908, -65.62736, -65.62443, -65.62410,
                               -65.62407, -65.62407, -65.62407, -65.62407, -65.62407), tolerance = 3e-7  )
)


stopifnot(
all.equal(t12$cov.estimates,
matrix(data = c(0.0023844118, -0.02009098, -0.002656031,  0.0015195246,  0.0009236147, -0.001211019,  0.0008305984,  0.0005869696, -0.0001436281,  0.001127857,
               -0.0200909828, 25.97675011,  0.432440287, -0.3705320089, -0.4059256158,  0.292658943,  0.1552722757, -0.0570759472,  0.2753255420, -0.501051481,
               -0.0026560314,  0.43244029,  0.522379702,  0.0058729610, -0.1217711181, -0.290424706, -0.1070016194, -0.1303433902, -0.0685981410, -0.119625632,
                0.0015195246, -0.37053201,  0.005872961,  0.1422915077, -0.0208217088, -0.169558793, -0.0008502762,  0.0464555294,  0.0430827345,  0.009537233,
                0.0009236147, -0.40592562, -0.121771118, -0.0208217088,  0.0835389458,  0.091147238,  0.0014369581, -0.0270061587, -0.0800881882,  0.090403298,
               -0.0012110187,  0.29265894, -0.290424706, -0.1695587930,  0.0911472377,  0.520446981,  0.0147750923, -0.0228003802, -0.0952969863,  0.018420162,
                0.0008305984,  0.15527228, -0.107001619, -0.0008502762,  0.0014369581,  0.014775092,  0.1140995146,  0.0165974067,  0.0706060631, -0.016286048,
                0.0005869696, -0.05707595, -0.130343390,  0.0464555294, -0.0270061587, -0.022800380,  0.0165974067,  0.1838633245,  0.1735945315, -0.046601444,
               -0.0001436281,  0.27532554, -0.068598141,  0.0430827345, -0.0800881882, -0.095296986,  0.0706060631,  0.1735945315,  0.3516947087, -0.128787828,
                0.0011278571, -0.50105148, -0.119625632,  0.0095372335,  0.0904032982,  0.018420162, -0.0162860481, -0.0466014445, -0.1287878276,  0.193288862),
        ncol = 10, nrow = 10, byrow = TRUE, dimnames = list( c("lambda", names(t12$params)), c("lambda", names(t12$params)) ) ), tolerance = 3e-7
)
)

stopifnot(
  all.equal(as.numeric(t12$std.error), c(0.04883044, 5.09673916, 0.72275840, 0.37721547, 0.28903105, 0.72142011, 0.33778620, 0.42879287, 0.59303854, 0.43964629), tolerance = 3e-7)
)

stopifnot(
  all.equal(as.numeric(t12$t.values), c(-4.088501,  2.485347,  5.964372, -0.376549,  3.416046,  2.556906, -0.889096,  7.627430), tolerance = 3e-7)
)


stopifnot(
  all.equal(as.numeric(t12$p.values), c(2.034929e-04, 1.722241e-02, 5.304351e-07, 7.084989e-01, 1.470696e-03, 1.445972e-02, 3.792674e-01, 2.546330e-09), tolerance = 3e-7)
)

stopifnot(
  all.equal(as.numeric(t12$ICs), c(151.2481, 156.8892, 170.3684), tolerance = 3e-7)
)


stopifnot(
  t12$ft == "nbRegMisrepEM"
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
test_data <- data.frame(Y = c(0,  1, 64,  1,  2,  2,  0,  0,  0,  0,  0,  0,  0,  4 , 2,  0,  2,  5,  0,  0,  1, 14,  0,  0,  0,  3,  1,  0,  4,  1,  0,  0,  5,  0,  0,  0,  0,
                              0,  0,  5,  3,  0,  0,  3, 81,  0,  0,  0, 29,  7),
                        X1 = c(1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0),
                        X2 = c(0.11386635,  1.47979040,  0.45636287, -0.41664853,  1.25417252,  2.10499692, -1.00587356, -0.55405844,  0.17479943,
                                -0.29340898, -0.05440845, -0.07675026, -1.89324885,  0.59106707, -0.65140908, -1.28337223, -0.48737945,  1.64559981,
                                -0.62364285,  0.79564381,  0.38869489,  1.61959941, -1.76141082,  0.37854440,  1.02329557,  0.74965937,  0.04413995,
                                -1.33996273,  2.08891011, -0.36087621,  0.32225472, -1.67851814,  0.41262611, -0.44329044, -0.30112059, -0.03306856,
                                 2.12324955,  0.54000803, -0.85397350, -0.40084273,  0.89826028, -0.04130717, -1.01038714,  0.83043124,  1.15626228,
                                -1.54063266, -0.88519104,  0.19762001, -0.09330770,  0.54228214),
                        X3 = c(0.9250996, 0.8397037, 0.8063966, 0.5005301, 0.5261619, 0.9748920, 0.7095294, 0.9362987, 0.4623964, 0.2624171, 0.7273491,
                                0.4308770, 0.1454707, 0.5222497, 0.9163159, 0.1999648, 0.7829910, 0.9217782, 0.5697253, 0.3976736, 0.8473057, 0.7532322,
                                0.8627066, 0.8478911, 0.3253753, 0.8730831, 0.5964159, 0.9204428, 0.3093212, 0.6506881, 0.8470737, 0.7918149, 0.1817754,
                                0.4743152, 0.8122764, 0.7745114, 0.8042454, 0.8288124, 0.5462465, 0.7267012, 0.8394176, 0.3295204, 0.6816037, 0.4984404,
                                0.4564431, 0.9894151, 0.8408505, 0.3310416, 0.5424446, 0.2656163),
                        Sex = c("Male",   "Female", "Male",   "Female", "Female", "Female", "Male",   "Female", "Male",   "Female", "Male",   "Female",
                                 "Female", "Female", "Male",   "Female", "Male",   "Male",   "Female", "Female", "Female", "Female", "Female", "Female",
                                 "Female", "Male",   "Male",   "Male",   "Male",   "Female", "Male",   "Male",   "Male",   "Male",   "Male",   "Female",
                                 "Male",   "Female", "Female", "Male",   "Male",   "Male",   "Female", "Male",   "Female", "Male",   "Male",   "Female",
                                 "Female", "Male"),
                        Race = c("Black", "Black", "Other", "Other", "White", "Black", "Black", "Black", "Black", "White", "White", "Other", "Black",
                                  "Black", "Black", "White", "Black", "White", "White", "Black", "Black", "Other", "White", "Other", "Black", "Black",
                                  "White", "Black", "Other", "Other", "White", "Black", "Other", "Other", "Black", "Black", "Other", "White", "White",
                                  "White", "Other", "Black", "Other", "Other", "White", "Black", "Black", "Black", "Other", "White"),
                        V_star = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1))

# Output needs to be a numeric vector
stopifnot(
  is.vector(predict(t12, test_data)) && is.numeric(predict(t12, test_data))
)


stopifnot(
  all.equal(predict(t12, test_data), c(1.324294995,  4.503639317,  2.893374359,  0.562230101,  1.961777902,  4.994997042,  0.079788289,  0.131666486,  0.653184221,  0.146261268,  0.617080347,
                                       0.403133484,  0.054754574,  3.580267897,  0.138970332,  0.026998283,  1.647697283, 10.968771040,  0.029818122,  1.561381116,  0.268308469, 17.562643287,
                                       0.003873439,  2.015000327,  0.923250557,  4.019055394,  2.557611800,  0.042357672, 55.245320731,  2.005485658,  0.447774566,  0.062487780,  3.179438139,
                                       5.787024250,  0.261486109,  1.139934031, 51.240735517,  0.527554062,  0.051518525,  2.927336122, 15.685656583,  0.466571492,  0.192324354,  5.995036976,
                                       14.554364558,  0.075112886,  0.094797836,  0.222069118,  8.365034279,  6.604011919), tolerance = 3e-7)
)


