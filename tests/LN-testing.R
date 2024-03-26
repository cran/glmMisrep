require(glmMisrep)

data <- data.frame( Y = c(76.537552,  2528.253825,   742.661850,   275.771394,   422.115809,   994.273666,    27.363153,   107.370891,
                            1133.422491,    42.577534,    21.222886,    82.130986,   254.636406,   116.754997,    96.768592,   238.197024,
                             617.472558,     9.391864,   100.894256,   332.483705,   279.115849,  1101.826204,   599.726263,   218.041193,
                             519.177473,  3529.107060,  1879.559865,    47.560367,   422.300405,   140.919276,   398.857500,  1003.204619,
                            1518.945852,   638.622736,  1667.131817,   352.307789,    86.629404,  5524.732901,   372.118071,  3732.461629,
                               4.532981,   147.234528,    41.538661, 10929.665862,    85.689014,    33.029995,    20.335263,    67.657641,
                             388.829832,     8.608040),
                    X1 = c(1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
                    X2 = c(0.49507310, -0.08883055,  0.12926013,  0.14710474, -0.51448308, -0.13046076,  2.08608211, -0.57426685, -0.90878741,
                             1.23110221,  1.67853057,  1.55074616, -2.18659493,  1.41014900,  0.58587594,  0.04134293, -1.99370526,  0.09277679,
                            -0.76394655, -1.28839675,  0.41072563, -1.27177378,  0.08823336,  0.20571840, -0.82168230,  0.33441449,  0.28506237,
                             0.04591112, -0.16366695, -1.45927411, -0.10830177, -1.29651305,  0.66361581,  2.00878945, -0.32495902, -1.13028382,
                             0.74934344, -0.54082773, -0.22820110, -0.67195420,  0.57810617,  1.16271770, -0.97158214,  0.85486460,  0.93789922,
                            -0.80378906,  0.55046386, -0.03277486,  1.10168928, -1.47985902),
                    X3 = c(0.2357360, 0.9084012, 0.9510478, 0.9426464, 0.5130887, 0.9366523, 0.2776905, 0.8125779, 0.8742008, 0.7971722, 0.3337451,
                            0.3999461, 0.8996933, 0.4550159, 0.4777070, 0.5786287, 0.9578764, 0.6710153, 0.6726491, 0.7067857, 0.9986008, 0.9106748,
                            0.1215141, 0.9760379, 0.8532801, 0.5979620, 0.9679438, 0.8752609, 0.4202729, 0.6799168, 0.9755574, 0.9049099, 0.9187066,
                            0.8170819, 0.5400875, 0.8945737, 0.1026229, 0.9990335, 0.7742958, 0.8731023, 0.3292357, 0.7081538, 0.6131705, 0.8676843,
                            0.7707079, 0.9935447, 0.8271532, 0.7889369, 0.6950691, 0.5327706),
                    Sex = c("Female", "Male",   "Female", "Male",   "Male",   "Male",   "Female", "Male",   "Male",   "Female", "Male",   "Female",
                            "Female", "Male",   "Female", "Male",   "Female", "Female", "Female", "Male",   "Female", "Male",   "Male",   "Male",
                             "Male",   "Female", "Female", "Male",   "Female", "Male",   "Female", "Female", "Male",   "Female", "Male",   "Female",
                             "Male",   "Male",   "Female", "Female", "Female", "Male",   "Male",   "Male",   "Female", "Female", "Female", "Female",
                             "Male",   "Female"),
                    Race = c("White", "Black", "Other", "White", "White", "Other", "Other", "White", "White", "White", "White", "Black", "Other", "White",
                              "White", "Other", "White", "Other", "Other", "Black", "Other", "Black", "Other", "Black", "Other", "Black", "Other", "Other",
                              "White", "White", "Other", "White", "Black", "Black", "Other", "Other", "White", "Other", "White", "Black", "Other", "Black",
                              "Black", "White", "Other", "Other", "Other", "White", "White", "Other"),
                    V_star = c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0))

data$Race <- as.factor(data$Race)
data$Sex <- as.factor(data$Sex)

t1 <- tryCatch(LnRegMisrepEM(formula = y ~ X1 + X2 + X3 + Sex + Race + V_star,
                                v_star = "V_star",
                                data = data,
                                lambda = c(0.6,0.4),
                                epsilon = 1e-08,
                                maxit = 10000,
                                maxrestarts = 20),
               error = function(x) x )

# The response above is inappropriately specified
stopifnot(
  t1$message == "formula must be 'log(response) ~ terms'. See 'Details'"
)


t2 <- tryCatch(LnRegMisrepEM(formula = log(y) ~ X1 + X2 + X3 + Sex + Race + V_star,
                             v_star = "V_star",
                             data = data,
                             lambda = c(0.6,0.4),
                             epsilon = 1e-08,
                             maxit = 10000,
                             maxrestarts = 20),
               error = function(x) x )

stopifnot(
  t2$message == "object 'y' not found"
)



t3 <- tryCatch(LnRegMisrepEM(formula = log(Y) ~ X1 + X2 + X3 + Sex + Race + V_star,
                                v_star = "V_Star",
                                data = data,
                                lambda = c(0.6,0.4),
                                epsilon = 1e-08,
                                maxit = 10000,
                                maxrestarts = 20),
               error = function(x) x )

# Argument to 'v_star' is misspelled
stopifnot(
  t3$message == "variable V_Star not present in dataframe"
)


data$V_star <- ifelse(data$V_star == 1, yes = "yes", no = "no")

t4 <- tryCatch(LnRegMisrepEM(formula = log(Y) ~ X1 + X2 + X3 + Sex + Race + V_star,
                                v_star = "V_star",
                                data = data,
                                lambda = c(0.6,0.4),
                                epsilon = 1e-08,
                                maxit = 10000,
                                maxrestarts = 20),
               error = function(x) x )


# v* variable is type character (yes and no)
stopifnot(
  t4$message == "v_star variable must be of class 'factor' or 'numeric'"
)

data$V_star <- ifelse(data$V_star == "yes", yes = 1, no = 0)


data$V_star[10] <- -1


t5 <- tryCatch(LnRegMisrepEM(formula = log(Y) ~ X1 + X2 + X3 + Sex + Race + V_star,
                                v_star = "V_star",
                                data = data,
                                lambda = c(0.6,0.4),
                                epsilon = 1e-08,
                                maxit = 10000,
                                maxrestarts = 20),
               error = function(x) x )

# v* variable must be binary
stopifnot(
  t5$message == "v_star variable must contain two unique values"
)

data$V_star[10] <- 0


data$V_star <- ifelse(data$V_star == 1, yes = 1, no = 2)

t6 <- tryCatch(LnRegMisrepEM(formula = log(Y) ~ X1 + X2 + X3 + Sex + Race + V_star,
                                v_star = "V_star",
                                data = data,
                                lambda = c(0.6,0.4),
                                epsilon = 1e-08,
                                maxit = 10000,
                                maxrestarts = 20),
               error = function(x) x )

# v* must be binary, but more specifically 0/1;
stopifnot(
  t6$message == "v_star variable must be coded with ones and zeroes"
)



data$V_star <- ifelse(data$V_star == 1, yes = 1, no = 0)

t7 <- tryCatch(LnRegMisrepEM(formula = log(Y) ~ X1 + X2 + X3 + Sex + Race + V_star,
                                v_star = "V_star",
                                data = data,
                                lambda = c(0.49, 0.52),
                                epsilon = 1e-08,
                                maxit = 10000,
                                maxrestarts = 20),
               error = function(x) x )

# Inappropriately specified lambda argument
stopifnot(
  t7$message == "Lambda vector must sum to one"
)


t8 <- tryCatch(LnRegMisrepEM(formula = log(Y) ~ X1 + X2 + X3 + Sex + Race + V_star,
                                v_star = "V_star",
                                data = data,
                                lambda = c(1/3, 1/3, 1/3),
                                epsilon = 1e-08,
                                maxit = 10000,
                                maxrestarts = 20),
               error = function(x) x )

# Inappropriately specified lambda argument
stopifnot(
  t8$message == "Lambda vector must contain two elements"
)



data$X4 <- data$X2*0.3

t9 <- tryCatch(LnRegMisrepEM(formula = log(Y) ~ X1 + X2 + X3 + X4 + Sex + Race + V_star,
                                v_star = "V_star",
                                data = data,
                                lambda = c(0.6, 0.4),
                                epsilon = 1e-08,
                                maxit = 10000,
                                maxrestarts = 20),
               error = function(x) x )

# Linearly dependent covariates/degenerate design matrix
stopifnot(
  t9$message == "Linear dependencies exist in the covariates"
)


t10 <- tryCatch(LnRegMisrepEM(formula = log(Y) ~ X1 + X2 + X3 + Sex + Race,
                             v_star = "V_star",
                             data = data,
                             lambda = c(0.6, 0.4),
                             epsilon = 1e-08,
                             maxit = 10000,
                             maxrestarts = 20),
               error = function(x) x )

# V_star variable is absent from formula argument
stopifnot(
  t10$message == "v_star variable must be specified in 'formula'"
)


# EM algorithm should fail to converge within the specified number of attempts
t11 <- tryCatch(
  capture.output(LnRegMisrepEM(formula = log(Y) ~ X1 + X2 + X3 + Sex + Race + V_star,
                               v_star = "V_star",
                               data = data,
                               lambda = c(0.6, 0.4),
                               epsilon = 1e-08,
                               maxit = 3,
                               maxrestarts = 4)),
  error = function(x) x
)

stopifnot(
  t11$message == "NOT CONVERGENT! Failed to converge after 4 attempts"
)



# On the first attempt, fails to converge, and restarts with new mixing props.
# Succeeds on the second attempt.
msg <- capture.output(
  t12 <-  LnRegMisrepEM(formula = log(Y) ~ X1 + X2 + X3 + Sex + Race + V_star,
                        v_star = "V_star",
                        data = data,
                        lambda = c(0.6, 0.4),
                        epsilon = 1e-08,
                        maxit = 15,
                        maxrestarts = 4, verb = TRUE),
  type = "message"
)

stopifnot(
  any(msg == "Warning: Failed to converge. Restarting with new mixing proportions")
)


# This should succeed;
msg <- capture.output(
 t13 <-  LnRegMisrepEM(formula = log(Y) ~ X1 + X2 + X3 + Sex + Race + V_star,
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
  is.list(t13)
)

# With 14 elements
stopifnot(
  length(t13) == 14
)

# Fisher information matrix should be symmetric
stopifnot(
  isSymmetric(t13$cov.estimates)
)


# The returned list should have elements with the following names
# and types
stopifnot(
  all.equal(lapply(t13, class),
            lapply(list(y = 0.1, lambda = 0.2, params = 0.3, loglik = 0.4,
                        posterior = 0.5, all.loglik = 0.6, cov.estimates = matrix(data = c(1,2,3,4), 2, 2),
                        std.error = 0.7, t.values = 0.8, p.values = 0.9, ICs = 1.0, ft = "*",
                        formula = y ~ x, v_star_name = "v*" ), class), tolerance = 1.5e-7 )
)


# Verifying the function can correctly calculate things;
stopifnot(
  all.equal(t13$lambda, 0.2752233, tolerance = 1.5e-7 )
)

stopifnot(
  all.equal(as.numeric(t13$params), c(0.38461784,  1.28998781,  2.23671321,  0.00136344,  3.43169080,  0.86482989, -0.87270674, -0.07989459,
                                      1.97303766), tolerance = 1.5e-7 )
)

stopifnot(
  all.equal( t13$loglik, -319.4672, tolerance = 1.5e-7 )
)

stopifnot(
  all.equal( t13$posterior, c(9.999942e-01, 9.998469e-01, 5.496038e-12, 9.998779e-01, 9.999986e-01, 5.904974e-09, 3.102086e-06,
                              9.999998e-01, 1.000000e+00, 9.999999e-01, 9.999998e-01, 4.037337e-05, 7.963463e-07, 1.392101e-01,
                              5.387680e-05, 9.999987e-01, 3.315679e-06, 1.000000e+00, 1.000000e+00, 3.769621e-02, 1.000000e+00,
                              1.000000e+00, 2.844998e-09, 9.999996e-01, 7.450721e-07, 5.085421e-10, 4.511226e-04, 1.000000e+00,
                              9.191679e-02, 9.972969e-01, 9.999983e-01, 9.997590e-01, 9.999999e-01, 1.050171e-08, 7.063420e-07,
                              9.999869e-01, 8.462061e-07, 1.071525e-04, 9.999998e-01, 6.965697e-05, 9.999988e-01, 9.995615e-01,
                              1.000000e+00, 1.176610e-06, 4.660242e-03, 1.000000e+00, 9.999999e-01, 9.998967e-01, 1.020901e-03,
                              9.999994e-01), tolerance = 1.5e-7 )
)


stopifnot(
  all.equal( t13$all.loglik, c(-340.5320, -325.7930, -321.3185, -320.3784, -320.0802, -319.8437, -319.7085, -319.6393, -319.5830, -319.5346,
                               -319.4999, -319.4805, -319.4718, -319.4687, -319.4677, -319.4674, -319.4673, -319.4673, -319.4672, -319.4672,
                               -319.4672, -319.4672, -319.4672, -319.4672), tolerance = 1.5e-7  )
)


stopifnot(
all.equal(t13$cov.estimates,
matrix(data = c(5.429249e-03, -2.887250e-04, -0.001407336,  8.894204e-06, -1.316386e-04,  0.001447803, -0.0001801113,  0.0001724132, -0.0007954676,  0.0001690426,
               -2.887250e-04,  1.768530e-03,  0.002022002, -1.978731e-06,  8.232676e-05, -0.001401457,  0.0001295077, -0.0002436579,  0.0006128995, -0.0001726792,
               -1.407336e-03,  2.022002e-03,  0.077937106, -5.957053e-03, -6.482111e-03, -0.060002084, -0.0106227091, -0.0225706061, -0.0218159265, -0.0098135914,
                8.894204e-06, -1.978731e-06, -0.005957053,  1.468143e-02,  9.544467e-04, -0.001405178, -0.0017195335,  0.0009178124,  0.0021212952,  0.0010653068,
               -1.316386e-04,  8.232676e-05, -0.006482111,  9.544467e-04,  4.629199e-03,  0.005663381,  0.0012697822,  0.0025368665,  0.0027705039, -0.0008591309,
                1.447803e-03, -1.401457e-03, -0.060002084, -1.405178e-03,  5.663381e-03,  0.070883845,  0.0025822106,  0.0050719148,  0.0054058102,  0.0043546425,
               -1.801113e-04,  1.295077e-04, -0.010622709, -1.719534e-03,  1.269782e-03,  0.002582211,  0.0151821526,  0.0045337746,  0.0015999026,  0.0001551407,
                1.724132e-04, -2.436579e-04, -0.022570606,  9.178124e-04,  2.536866e-03,  0.005071915,  0.0045337746,  0.0243451893,  0.0165216203, -0.0003004559,
               -7.954676e-04,  6.128995e-04, -0.021815927,  2.121295e-03,  2.770504e-03,  0.005405810,  0.0015999026,  0.0165216203,  0.0289331944,  0.0009597426,
                1.690426e-04, -1.726792e-04, -0.009813591,  1.065307e-03, -8.591309e-04,  0.004354642,  0.0001551407, -0.0003004559,  0.0009597426,  0.0129483355),
        ncol = 10, nrow = 10, byrow = TRUE, dimnames = list( c("lambda", names(t13$params)), c("lambda", names(t13$params)) ) ), tolerance = 1.5e-7
)
)

stopifnot(
  all.equal(as.numeric(t13$std.error), c(0.07368344, 0.04205390, 0.27917218, 0.12116695, 0.06803822, 0.26624020, 0.12321588, 0.15602945, 0.17009760, 0.11379075), tolerance = 1.5e-7)
)

stopifnot(
  all.equal(as.numeric(t13$t.values), c(4.62076060, 18.45976267,  0.02003933, 12.88945387,  7.01881857, -5.59321807, -0.46969848, 17.33917394), tolerance = 1.5e-7)
)


stopifnot(
  all.equal(as.numeric(t13$p.values), c(3.930521e-05, 3.541604e-21, 9.841116e-01, 7.954660e-16, 1.765708e-08, 1.761973e-06, 6.411210e-01, 3.304182e-20), tolerance = 1.5e-7)
)

stopifnot(
  all.equal(as.numeric(t13$ICs), c(658.9345, 664.5755, 678.0547), tolerance = 1.5e-7)
)


stopifnot(
  t13$ft == "LnRegMisrepEM"
)

stopifnot(
  class(t13$formula) == "formula"
)

stopifnot(
  t13$v_star_name == "V_star"
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

test_data <- data.frame(Y = c(98.427279,    32.777322,  5486.156939,  1339.276500,   312.445342,  1718.322835,   185.328612,  1734.535647,
                                3971.255712,  3834.522975,    88.011945,   418.930890,  1108.338108,   188.514665,    30.939923,   226.444696,
                                  10.108475,    59.441742,   120.989164,  1554.964939, 27958.871182, 29334.748743,   792.590049,    33.450942,
                                   7.665141,   175.831588,   280.975447,    33.607044,    23.775642,   218.677457,     6.241764,   158.587975,
                                  13.852720,  2025.885032,  3070.340378,   250.562014,     3.167028,   412.431731,   253.282068,    30.773635,
                                  84.806336,    16.176141,  2544.437710,   326.410467,  1331.172612,     8.942180, 12994.689597,    82.674185,
                                  64.729101,    14.448531),
                        X1 = c(0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0),
                        X2 = c(-1.47112660,  0.55436293, -0.66931369,  0.41044485,  1.09693026, -0.09962265, -1.20740417, -0.58021023, -0.01312526,
                                -1.02177231,  0.21622835, -0.43729831,  0.61557692, -2.40180053,  1.16981893,  0.55783770,  0.03185095,  1.86912879,
                                -0.50083512, -0.60316555, -0.05168889, -0.88313525, -0.69394152, -0.53436444,  0.67535031, -0.78427798, -0.86134004,
                                -0.60073262, -1.14478702, -1.77009228, -0.18038588, -0.31001308, -2.82399557,  0.61107096, -1.36033114,  0.37042505,
                                -0.93805180,  0.48696814,  0.58320970, -0.09727662,  1.91523731, -1.94005641,  1.01199900, -0.32069647,  0.02523619,
                                 0.36135866,  0.16991146,  0.93392489,  0.34386311, -0.69329227),
                        X3 = c(0.8830248, 0.9319574, 0.9654841, 0.9664790, 0.5306699, 0.9716790, 0.7541325, 0.9479806, 0.8758044, 0.4956611, 0.1328917,
                                0.6745790, 0.8658182, 0.9922382, 0.4903624, 0.6850327, 0.3320028, 0.5798331, 0.7594182, 0.8087243, 0.9985892, 0.9583971,
                                0.5088030, 0.7562532, 0.6356310, 0.8385115, 0.7309135, 0.3821138, 0.5828014, 0.6764654, 0.4884371, 0.7026958, 0.4908599,
                                0.3674397, 0.9896373, 0.7971634, 0.1971907, 0.8849176, 0.7674014, 0.7965604, 0.4593090, 0.5414791, 0.6594986, 0.8939997,
                                0.8013125, 0.7305848, 0.8602200, 0.5298325, 0.3602862, 0.7062794),
                        Sex = c("Female", "Female", "Male",   "Female", "Male",   "Female", "Male",   "Female", "Male",   "Male",   "Male",   "Female",
                                 "Male",   "Male",   "Male",   "Male",   "Female", "Female", "Male",   "Female", "Male",   "Male",   "Female", "Male",
                                 "Female", "Male",   "Male",   "Female", "Male",   "Male",   "Female", "Female", "Female", "Male",   "Male",   "Male",
                                 "Male",   "Female", "Male",   "Male",   "Female", "Male",   "Female", "Male",   "Male",   "Female", "Male",   "Female",
                                 "Female", "Female"),
                        Race = c("Other", "Other", "Other", "Other", "Other", "Black", "Black", "Black", "Other", "White", "Black", "White", "Black", "Other",
                                  "White", "Other", "Other", "Other", "White", "Other", "White", "White", "Black", "Other", "Other", "White", "Other", "Other",
                                  "Other", "Other", "Other", "White", "Black", "White", "Black", "Black", "Other", "White", "Other", "Other", "Other", "Other",
                                  "White", "Other", "Black", "Other", "White", "Other", "Black", "Other"),
                        V_star = c(1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0))

# Output needs to be a numeric vector
stopifnot(
  is.vector(predict(t13, test_data)) && is.numeric(predict(t13, test_data))
)


stopifnot(
  all.equal(predict(t13, test_data), c(5.344616, 4.085287, 8.730227, 6.440271, 5.003774, 7.330128, 5.210181, 7.248147, 6.993357, 7.910271, 5.316929, 6.230217, 7.832651, 5.152951, 4.228351, 6.339466, 2.025716,
                                       5.115411, 5.149389, 7.327536, 8.207476, 9.498427, 7.170882, 4.345670, 3.068551, 5.420426, 6.494979, 2.196819, 3.749605, 6.306891, 2.562261, 6.326879, 3.439677, 7.472481,
                                       8.254866, 5.360002, 2.426590, 6.953294, 6.622164, 4.484588, 4.701873, 3.606715, 7.610454, 7.055378, 5.373769, 3.393975, 9.162949, 4.942549, 4.425920, 3.309129), tolerance = 1.5e-7)
)







