require(glmMisrep)

data <- data.frame( Y = c(0,   0,   2,   0,   3,   0,  36,   0,   2,   1,   0,   2,   6,   9,   0,   0,   0,   0,   7,   1,   1,   2,  50,   4,   0,   0,   0,   1,
                          0,   0,   0,   3,   0,   0,   1,   0,   1,   3, 176,   0,   0,   0,   0,   0,   2, 286,   0,   0,   0,  18),
                    X1 = c(0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1),
                    X2 = c(1.71870212, -0.55840901,  1.22589915,  0.53000107,  0.62571132,  0.02873955,  0.30989954,  1.35514993,  0.15587503,
                             0.27987513,  0.48892178,  0.35218767,  0.52382778,  1.58126751, -0.07855081, -0.57128802, -0.92500953, -2.48543328,
                             0.03810910,  0.39929906, -0.54854763, -0.10505694,  0.45120734,  0.32295222, -0.68595918, -0.66892486,  1.72253431,
                            -0.28425276, -0.67719912, -0.39644260, -0.16843500,  0.90540261, -1.38574804,  0.14456841,  0.44142810, -1.89442541,
                            -0.65961894,  2.13148776,  1.72410805, -1.60207312, -1.09525034, -1.31327168, -0.43378445,  1.33644956,  1.28938359,
                             0.90232362, -0.94112768, -0.61851917,  0.37033085, -0.47019541),
                    X3 = c(0.6882029, 0.9934165, 0.9173388, 0.9406660, 0.5130041, 0.8590187, 0.4468488, 0.4186652, 0.5098278, 0.3339481, 0.6922477,
                            0.6793977, 0.4983724, 0.6079911, 0.7763041, 0.8529067, 0.8287771, 0.9125900, 0.4802076, 0.8981448, 0.3570093, 0.9209584,
                            0.4353817, 0.9426418, 0.9550002, 0.4869851, 0.9560156, 0.8247537, 0.1939687, 0.6103839, 0.7721900, 0.5980044, 0.8683831,
                            0.7004518, 0.8577210, 0.3576712, 0.9540088, 0.9880046, 0.4304899, 0.7416618, 0.6656063, 0.8920356, 0.6097593, 0.8008748,
                            0.8110432, 0.5967969, 0.6983106, 0.9471680, 0.9929186, 0.9059200),
                    Sex = c("Female", "Male",   "Male",   "Female", "Male",   "Female", "Male",   "Male",   "Female", "Female", "Male",   "Male",
                             "Male",   "Female", "Male",   "Male",   "Female", "Female", "Male",   "Female", "Female", "Male",   "Female", "Male",
                             "Male",   "Male",   "Male",   "Female", "Female", "Female", "Male",   "Male",   "Male",   "Male",   "Female", "Female",
                             "Female", "Female", "Male",   "Male",   "Male",   "Male",   "Male",   "Male",   "Female", "Male",   "Male",   "Female",
                             "Female", "Male"),
                    Race = c("Black", "White", "White", "White", "White", "White", "Black", "Black", "Black", "Black", "Black", "Black", "White", "Other",
                              "Black", "White", "Black", "White", "Other", "Black", "White", "Other", "Other", "White", "White", "White", "Black", "Other",
                              "White", "Other", "White", "Black", "Other", "Black", "Other", "Other", "Other", "Black", "Other", "Black", "White", "Other",
                              "Black", "White", "Black", "Other", "White", "Black", "Black", "Other"),
                    V_star = c(0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0))

data$Race <- as.factor(data$Race)
data$Sex <- as.factor(data$Sex)

t1 <- tryCatch(poisRegMisrepEM(formula = y ~ X1 + X2 + X3 + Sex + Race + V_star,
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


t2 <- tryCatch(poisRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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

t3 <- tryCatch(poisRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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


t4 <- tryCatch(poisRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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

t5 <- tryCatch(poisRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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

t6 <- tryCatch(poisRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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


t7 <- tryCatch(poisRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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

t8 <- tryCatch(poisRegMisrepEM(formula = Y ~ X1 + X2 + X3 + X4 + Sex + Race + V_star,
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


t9 <- tryCatch(poisRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race,
                               v_star = "V_star",
                               data = data,
                               lambda = c(0.6, 0.4),
                               epsilon = 1e-08,
                               maxit = 10000,
                               maxrestarts = 20),
               error = function(x) x )

#V_star variable absent from formula
stopifnot(
  t9$message == "v_star variable must be specified in 'formula'"
)



# EM algorithm should fail to converge within the specified number of attempts
t10 <- tryCatch(
  capture.output(poisRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
                                  v_star = "V_star",
                                  data = data,
                                  lambda = c(0.6, 0.4),
                                  epsilon = 1e-08,
                                  maxit = 3,
                                  maxrestarts = 4)),
  error = function(x) x
)

stopifnot(
  t10$message == "NOT CONVERGENT! Failed to converge after 4 attempts"
)


# On the first attempt, fails to converge, and restarts with new mixing props.
# Succeeds on the second attempt.
msg <- capture.output(
  t11 <-  poisRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
                           v_star = "V_star",
                           data = data,
                           lambda = c(0.6, 0.4),
                           epsilon = 1e-08,
                           maxit = 16,
                           maxrestarts = 4, verb = TRUE),
  type = "message"
)

stopifnot(
  any(msg == "Warning: Failed to converge. Restarting with new mixing proportions")
)


msg <- capture.output(
 t12 <-  poisRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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
                        std.error = 0.7, z.values = 0.8, p.values = 0.9, ICs = 1.0, ft = "*",
                        formula = y ~ x, v_star_name = "v*" ), class) )
)


# Verifying the function can correctly calculate things;
stopifnot(
  all.equal(t12$lambda,  0.2140361, tolerance = 2e-7 )
)

stopifnot(
  all.equal(as.numeric(t12$params), c(-1.1486407,  1.9813423,  1.0606001, -4.0278727,  1.8886687,  2.1052795, -0.1815366,  2.2582766), tolerance = 2e-7 )
)

stopifnot(
  all.equal( t12$loglik, -77.17481, tolerance = 2e-7 )
)

stopifnot(
  all.equal( t12$posterior, c(9.131063e-01,  9.167867e-01,  9.987564e-01,  8.007038e-01,  1.000000e+00,  7.980578e-01,  1.761218e-22,  9.999998e-01,
                              5.707215e-02,  4.986677e-01,  9.591568e-01,  9.998818e-01,  1.596141e-04,  1.000000e+00,  8.834283e-01,  8.269226e-01,
                              8.267814e-01,  7.909421e-01,  9.995369e-01,  3.002507e-01,  7.724813e-01,  5.033734e-01,  1.413033e-29,  7.033365e-04,
                              9.184187e-01,  9.118807e-01,  9.752457e-01,  4.103920e-01,  8.589374e-01,  9.278243e-01,  8.652112e-01,  1.000000e+00,
                              9.998403e-01,  9.999676e-01,  5.418147e-01,  8.817133e-01,  3.273919e-01,  1.253503e-01, 2.099810e-102,  8.124995e-01,
                              8.350465e-01,  9.998171e-01,  9.066149e-01,  1.000000e+00,  4.335937e-01, 2.097168e-171,  8.364673e-01,  7.911482e-01,
                              7.981111e-01,  1.769426e-10), tolerance = 2e-7 )
)


stopifnot(
  all.equal( t12$all.loglik, c(-91.21048, -77.54635, -77.24049, -77.18672, -77.17811, -77.17606, -77.17532, -77.17502, -77.17490, -77.17484, -77.17482,
                               -77.17481, -77.17481, -77.17481, -77.17481, -77.17481, -77.17481, -77.17481, -77.17481, -77.17481, -77.17481), tolerance = 2e-7  )
)


stopifnot(
all.equal(t12$cov.estimates,
matrix(data = c(0.0114492285, -0.0004087145,  0.0016883055, -0.001674646, -0.01493787,  0.004194090,  0.004620007,  0.003212934,  0.0008121915,
               -0.0004087145,  0.1376023242, -0.0103938967, -0.021763262, -0.10804979,  0.001850089, -0.001523425, -0.033947804, -0.0502985311,
                0.0016883055, -0.0103938967,  0.0153809909,  0.005402047, -0.02063417,  0.003663544,  0.001754054,  0.008152043,  0.0006989913,
               -0.0016746456, -0.0217632624,  0.0054020474,  0.012549504,  0.02752248, -0.006829300, -0.008499404,  0.001601437,  0.0045586028,
               -0.0149378678, -0.1080497865, -0.0206341716,  0.027522475,  0.26797912, -0.040935476, -0.039438494, -0.023606980,  0.0234374153,
                0.0041940900,  0.0018500887,  0.0036635440, -0.006829300, -0.04093548,  0.024832614,  0.009305140,  0.004723165, -0.0062878481,
                0.0046200066, -0.0015234254,  0.0017540537, -0.008499404, -0.03943849,  0.009305140,  0.033156029,  0.023238618, -0.0090039233,
                0.0032129343, -0.0339478042,  0.0081520434,  0.001601437, -0.02360698,  0.004723165,  0.023238618,  0.095149535,  0.0125163473,
                0.0008121915, -0.0502985311,  0.0006989913,  0.004558603,  0.02343742, -0.006287848, -0.009003923,  0.012516347,  0.0479783934),
        ncol = 9, nrow = 9, byrow = TRUE, dimnames = list( c("lambda", names(t12$params)), c("lambda", names(t12$params)) ) ), tolerance = 2e-7
)
)

stopifnot(
  all.equal(as.numeric(t12$std.error), c(0.1070011, 0.3709479, 0.1240201, 0.1120246, 0.5176670, 0.1575837, 0.1820880, 0.3084632, 0.2190397), tolerance = 2e-7)
)

stopifnot(
  all.equal(as.numeric(t12$z.values), c(-3.0965016, 15.9759747,  9.4675669, -7.7808179, 11.9851800, 11.5618810, -0.5885196, 10.3098958), tolerance = 2e-7)
)


stopifnot(
  all.equal(as.numeric(t12$p.values), c(1.958188e-03, 1.878943e-57, 2.864376e-21, 7.205715e-15, 4.249202e-33, 6.428496e-31, 5.561836e-01, 6.357023e-25), tolerance = 2e-7)
)

stopifnot(
  all.equal(as.numeric(t12$ICs), c(172.3496, 176.8496, 189.5578), tolerance = 2e-7)
)


stopifnot(
  t12$ft == "poisRegMisrepEM"
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
  all.equal(predict(t12, test_data), c(1.170566e+00, 1.063411e+00, 3.073438e+00, 4.579298e+00, 2.468377e+00, 1.650861e-01, 1.172834e-01, 8.335130e-02, 1.110133e+00, 1.383375e+00,
                                       1.811205e+00, 1.198719e+00, 2.266651e-01, 6.927864e-01, 7.426417e-02, 6.225827e-01, 5.104829e-01, 5.022287e+00, 3.897401e-02, 3.053586e+00,
                                       4.470380e-02, 1.434410e+01, 3.582593e-03, 2.626893e+00, 7.172219e-01, 2.832969e+00, 1.586127e+00, 3.518873e-02, 1.285648e+02, 1.235260e+00,
                                       2.299033e-01, 2.991943e-01, 3.631766e+01, 1.522515e+01, 1.637273e-01, 1.293711e-01, 1.816255e+01, 3.420085e-01, 2.433543e-01, 4.245742e+00,
                                       3.118107e+01, 1.507549e+00, 1.176431e+00, 1.579893e+01, 9.947078e+00, 1.562451e-01, 7.854286e-02, 2.920283e-01, 1.840087e+01, 1.019649e+01), tolerance = 2e-7)
)





