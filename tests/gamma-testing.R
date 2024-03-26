require(glmMisrep)

data <- data.frame( Y = c(45.857704, 26.966636, 1058.634073, 623.319053, 1087.177701, 19.323827, 86.047383, 606.742023,
                      1647.937035, 266.180140, 157.192319, 281.382229, 648.672212, 140.771360, 102.904828, 380.052888,
                        22.064627, 208.741717, 67.074619, 174.856091, 31.569650, 626.408259, 993.658540, 189.571355,
                       374.603446, 1802.252002, 543.301728, 158.015234, 104.378652, 582.149021, 52.249839, 45.702009,
                     11806.824361, 10053.322011,  1.087628, 55.884707, 271.186925, 178.080733, 563.668826, 1596.744067,
                         8.947570, 398.868445, 39.580796, 1293.273355, 23.025039, 184.609120, 238.859666, 169.540935,
                      775.025538, 4.616906),
                    X1 = c(1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1,
                           0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0),
                    X2 = c(-1.072049676,  0.727369558,  1.420449831,  0.919953091,  0.274830496,  0.546589583,  0.766522073,  0.102791747,
                             0.326183969,  1.929334793, -0.843138417, -0.064225873,  0.256683194,  0.625580034,  0.469241089,  0.255148171,
                            -0.235947534,  1.816474277, -1.692768826, -1.068503018, -0.137072147,  0.772976621, -0.719329599, -1.581414358,
                             0.049968814,  0.184292275, -1.321527190,  0.593936587, -1.441344834,  0.493651003, -0.468055987, -0.312559805,
                            -0.196035448, -0.541855456,  1.913445157,  0.403104413, -1.503943713,  0.401514018,  1.613023384, -1.063038013,
                             0.414808888,  0.417654550,  0.410841245,  0.218083137, -0.462372658, -0.908533492,  0.157572760,  0.750073708,
                             0.005803132,  0.152076943),
                    X3 = c(0.48973164, 0.69612108, 0.81936453, 0.83451060, 0.97515909, 0.88015868, 0.88890374, 0.37991780, 0.89681143, 0.99601041,
                            0.67524620, 0.92379599, 0.85120739, 0.41159392, 0.53689827, 0.77538594, 0.19803702, 0.95549912, 0.14601890, 0.80506128,
                            0.12918729, 0.89932864, 0.74272902, 0.47642162, 0.29003290, 0.61241891, 0.62205612, 0.77209866, 0.83853287, 0.80933236,
                            0.42349111, 0.81845100, 0.95771341, 0.73298222, 0.02518371, 0.63923492, 0.54736138, 0.57766185, 0.64674272, 0.54556027,
                            0.76556145, 0.74711758, 0.74089894, 0.99431826, 0.49083626, 0.27663899, 0.85629864, 0.71527708, 0.62011519, 0.25683099),
                    Sex = c("Female", "Female", "Male",   "Male",   "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Male",
                            "Female", "Male",   "Male",   "Female", "Female", "Male",   "Male",   "Female", "Female", "Female", "Male",   "Male",
                            "Male",   "Female", "Male",   "Female", "Male",   "Female", "Male",   "Male",   "Male",   "Male",   "Female", "Female",
                            "Female", "Female", "Male",   "Male",   "Female", "Female", "Male",   "Female", "Male",   "Male",   "Male",   "Female",
                            "Male",   "Female"),
                    Race = c("Other", "White", "Black", "White", "White", "Other", "White", "White", "Other", "Other", "Other", "Black", "White", "Other",
                              "Black", "Black", "Black", "Black", "White", "Black", "White", "White", "Black", "White", "Black", "Black", "Black", "Other",
                              "Other", "Black", "White", "Other", "Black", "White", "Other", "White", "White", "Black", "Black", "White", "Other", "White",
                              "Black", "Other", "White", "White", "White", "White", "Black", "Black"),
                    V_star = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) )

data$Race <- as.factor(data$Race)
data$Sex <- as.factor(data$Sex)

t1 <- tryCatch(gammaRegMisrepEM(formula = y ~ X1 + X2 + X3 + Sex + Race + V_star,
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


t2 <- tryCatch(gammaRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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

t3 <- tryCatch(gammaRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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


t4 <- tryCatch(gammaRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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

t5 <- tryCatch(gammaRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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

t6 <- tryCatch(gammaRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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


t7 <- tryCatch(gammaRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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

t8 <- tryCatch(gammaRegMisrepEM(formula = Y ~ X1 + X2 + X3 + X4 + Sex + Race + V_star,
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

t9 <- tryCatch(gammaRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race,
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
  capture.output(gammaRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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
 t11 <-  gammaRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
                                v_star = "V_star",
                                data = data,
                                lambda = c(0.6, 0.4),
                                epsilon = 1e-08,
                                maxit = 30,
                                maxrestarts = 4, verb = TRUE),
 type = "message"
  )

stopifnot(
  any(msg == "Warning: Failed to converge. Restarting with new mixing proportions")
)


# This should succeed;
msg <- capture.output(
  t12 <-  gammaRegMisrepEM(formula = Y ~ X1 + X2 + X3 + Sex + Race + V_star,
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
                        formula = y ~ x, v_star_name = "v*" ), class), tolerance = 1.5e-7 )
)


# Verifying the function can correctly calculate things;
stopifnot(
  all.equal(t12$lambda, 0.1079016 , tolerance = 1.5e-7)
)

stopifnot(
  all.equal(as.numeric(t12$params), c(6.14834270,  1.11254217,  1.85164785, -0.05971879,  3.68687601,  1.03378987, -1.03786486,  0.01177176,  2.11841547), tolerance = 1.5e-7 )
)

stopifnot(
  all.equal( t12$loglik, -303.1538, tolerance = 1.5e-7 )
)

stopifnot(
  all.equal( t12$posterior, c(9.999299e-01, 9.999881e-01, 9.999295e-01, 2.103412e-02, 9.988509e-01, 9.999868e-01, 9.998953e-01, 3.558117e-12,
                              3.158194e-15, 1.047717e-10, 3.497216e-21, 9.999045e-01, 6.002373e-16, 5.375916e-19, 9.972457e-01, 3.068774e-11,
                              9.999950e-01, 9.999795e-01, 9.999914e-01, 9.999972e-01, 5.394296e-09, 2.489884e-12, 9.998797e-01, 9.999950e-01,
                              1.426431e-29, 2.989857e-17, 9.999592e-01, 9.995238e-01, 9.993700e-01, 1.407886e-17, 9.997910e-01, 9.999866e-01,
                              6.992912e-09, 5.881790e-22, 9.999294e-01, 9.963445e-01, 9.976430e-01, 9.998867e-01, 9.999221e-01, 1.061977e-02,
                              9.999959e-01, 9.996503e-01, 9.999986e-01, 1.269408e-05, 9.999974e-01, 9.998703e-01, 9.998375e-01, 9.999912e-01,
                              9.993344e-01, 9.999934e-01), tolerance = 1.5e-7 )
)


stopifnot(
  all.equal( t12$all.loglik, c(-323.9350, -307.5493, -305.8435, -303.7539, -303.4699, -303.4653, -303.4630, -303.4617, -303.4607, -303.4600,
                               -303.4595, -303.4590, -303.4586, -303.4581, -303.4577, -303.4573, -303.4568, -303.4562, -303.4555, -303.4546,
                               -303.4534, -303.4517, -303.4491, -303.4447, -303.4369, -303.4219, -303.3919, -303.3337, -303.2463, -303.1768,
                               -303.1562, -303.1540, -303.1538, -303.1538, -303.1538, -303.1538, -303.1538), tolerance = 1.5e-7  )
)


stopifnot(
all.equal(t12$cov.estimates,
matrix(data = c(2.630725e-03,  0.0006112473,  0.0001634518,  5.617343e-05, -6.832485e-05, -0.0001355185, -0.0001500114, -4.349024e-05, -0.0001856351,  8.435390e-06,
                6.112473e-04,  1.5012984017,  0.0093211267,  2.562910e-03, -3.769762e-03, -0.0070031944, -0.0080879663, -2.985069e-03, -0.0090989843,  1.480260e-03,
                1.634518e-04,  0.0093211267,  0.0536024555, -9.696850e-03, -3.032083e-03, -0.0396014536, -0.0160312824, -1.492919e-02, -0.0164256693, -7.126126e-03,
                5.617343e-05,  0.0025629100, -0.0096968496,  1.537242e-02,  2.629801e-03, -0.0022881318,  0.0023638365,  4.015305e-03,  0.0016797283,  2.267238e-03,
               -6.832485e-05, -0.0037697616, -0.0030320829,  2.629801e-03,  5.854989e-03, -0.0029843267,  0.0031799149,  2.419577e-03,  0.0034715873,  2.819921e-05,
               -1.355185e-04, -0.0070031944, -0.0396014536, -2.288132e-03, -2.984327e-03,  0.0622968809,  0.0022770254, -2.445532e-03,  0.0006865788, -7.372706e-04,
               -1.500114e-04, -0.0080879663, -0.0160312824,  2.363836e-03,  3.179915e-03,  0.0022770254,  0.0176896392,  7.219233e-03,  0.0072975355,  2.140232e-03,
               -4.349024e-05, -0.0029850693, -0.0149291928,  4.015305e-03,  2.419577e-03, -0.0024455320,  0.0072192326,  2.655431e-02,  0.0123892138, -3.532181e-04,
               -1.856351e-04, -0.0090989843, -0.0164256693,  1.679728e-03,  3.471587e-03,  0.0006865788,  0.0072975355,  1.238921e-02,  0.0217022826,  9.302658e-04,
                8.435390e-06,  0.0014802596, -0.0071261264,  2.267238e-03,  2.819921e-05, -0.0007372706,  0.0021402318, -3.532181e-04,  0.0009302658,  1.533717e-02),
        ncol = 10, nrow = 10, byrow = TRUE, dimnames = list( c("lambda", names(t12$params)), c("lambda", names(t12$params)) ) ), tolerance = 1.5e-7
)
)

stopifnot(
  all.equal(as.numeric(t12$std.error), c(0.05129059, 1.22527483, 0.23152204, 0.12398556, 0.07651790, 0.24959343, 0.13300240, 0.16295494, 0.14731695, 0.12384335), tolerance = 1.5e-7)
)

stopifnot(
  all.equal(as.numeric(t12$t.values), c(4.80534019, 14.93438301, -0.78045509, 14.77152660,  7.77271573, -6.36902979,  0.07990771, 17.10560594), tolerance = 1.5e-7)
)


stopifnot(
  all.equal(as.numeric(t12$p.values), c(2.197068e-05, 5.930384e-18, 4.397174e-01, 8.610197e-18, 1.611142e-09, 1.431638e-07, 9.367091e-01, 5.337355e-20), tolerance = 1.5e-7)
)

stopifnot(
  all.equal(as.numeric(t12$ICs), c(626.3076, 631.9486, 645.4279), tolerance = 1.5e-7)
)


stopifnot(
  t12$ft == "gammaRegMisrepEM"
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
test_data <- data.frame(Y = c(868.918021, 19.678007, 11.639540, 2874.364481, 7.390559, 43.761220, 38.527232, 207.544958,
                                 315.480351, 89.784945, 716.564560, 283.222002, 110.361662, 451.436441, 39.953629, 233.833654,
                                1617.748394, 75.500771, 50.563671, 377.819000, 61.880717, 182.518634, 59.918501, 631.433665,
                                 227.032470, 1208.470445, 22297.217699, 99.277235, 48.747687, 567.238722, 16.189474, 59.755615,
                                 488.582967, 72.578502, 109.000453, 27.940203, 848.078128, 213.259038, 1682.801250, 190.297508,
                                 402.283888, 337.186677, 93.907346, 2641.757569, 7.341533, 741.742028, 280.497774, 158.552881,
                                1001.310106, 1.365170),
                        X1 = c(1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
                               1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                        X2 = c(-0.04036831,  0.79215827, -0.78625808, -0.62842526,  0.71228752,  1.01198145,  0.93175448,  0.86421429,  0.91651601,
                                 0.03113328,  0.19542685, -0.90669877,  0.37619711, -1.35330291,  0.89398740,  1.35171344,  0.88679687,  0.24370426,
                                -0.36203626, -1.77181974,  1.55322455, -0.56223819, -0.81659026, -1.73245607,  0.84049303, -1.22086244,  0.60102436,
                                 0.77569602, -1.64647498, -1.30859640,  1.96115643, -0.96724573, -0.93392441,  1.34514202,  0.28529075, -0.85545022,
                                -1.33833322, -0.96747450,  0.89774060,  0.85316222, -0.78181761, -1.03663465,  0.03445557, -0.62523669, -0.71794482,
                                 0.39289925,  1.40083561, -0.86121441, -0.51430495, -0.96633932),
                        X3 = c(0.9917792, 0.5148429, 0.3312061, 0.8199747, 0.6807638, 0.2200620, 0.3448447, 0.6674311, 0.8153877, 0.8780843, 0.6615473,
                                0.5310375, 0.9079096, 0.7269245, 0.7921638, 0.5748487, 0.8824124, 0.4347294, 0.6486300, 0.7234909, 0.4822567, 0.5384035,
                                0.7392369, 0.6618452, 0.8844936, 0.8918607, 0.9462929, 0.6252829, 0.7292003, 0.5072870, 0.6786043, 0.3788553, 0.9791500,
                                0.8192261, 0.7201488, 0.5233121, 0.7907446, 0.9223334, 0.9644428, 0.8472807, 0.6371841, 0.8956768, 0.9222600, 0.8891910,
                                0.4631308, 0.9567973, 0.8426664, 0.5904602, 0.7635194, 0.3655657),
                        Sex = c("Female", "Male",   "Female", "Male",   "Female", "Female", "Female", "Male",   "Female", "Female", "Male",   "Male",
                                 "Male",   "Female", "Female", "Female", "Male",   "Male",   "Female", "Female", "Male",   "Female", "Female", "Male",
                                 "Male",   "Female", "Male",   "Male",   "Male",   "Male",   "Female", "Male",   "Male",   "Female", "Male",   "Male",
                                 "Female", "Female", "Male",   "Male",   "Female", "Female", "Male",   "Male",   "Female", "Female", "Male",   "Female",
                                 "Female", "Female"),
                        Race = c("White", "White", "Black", "Black", "White", "White", "Black", "Other", "Other", "Black", "White", "White", "Black", "White",
                                  "White", "White", "White", "Other", "Black", "Black", "Black", "White", "White", "White", "Black", "White", "White", "Black",
                                  "White", "Black", "Other", "Black", "Other", "Black", "Black", "Other", "Other", "Other", "Black", "White", "Black", "Black",
                                  "Other", "White", "Black", "Black", "White", "White", "Black", "Other"),
                        V_star = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0))

# Output needs to be a numeric vector
stopifnot(
  is.vector(predict(t12, test_data)) && is.numeric(predict(t12, test_data))
)


stopifnot(
  all.equal(predict(t12, test_data), c(1362.316481,    98.589164,    19.348497,  1518.413557,    64.953463,    74.356504,   116.973660,   384.230744,   235.056907,   138.389645,  1117.809473,   538.364392,
                                       425.480229,   554.941246,    96.886303,   269.518581,  1766.765845,   169.077478,    60.799604,   555.253377,    82.566032,   264.163246,    88.283820,  1255.583105,
                                       379.615692,  1011.361154, 14488.916732,   146.547747,   251.364162,   684.325927,    20.936451,    65.555011,  1350.047237,   102.986646,   214.090005,    39.288930,
                                       245.581397,   284.689203,  3236.187735,   334.613196,   380.733764,   157.385844,   162.174052,  1982.659915,    31.340938,   841.416844,   318.383453,   237.724783,
                                       435.577471,     7.863122), tolerance = 1.5e-7)
)


