# UPDATED 7 MAR 2006 TO INCLUDE INTERACTION IN MODEL 3

##############################################################################

# Fit the simulated examples in R

# Read in data for Examples 1-3

y123 <- read.table ("y123.dat", header=TRUE)
y <- y123$y
x <- y123$x
group <- y123$group

u <- read.table ("u.dat", header=TRUE)$u

##############################################################################

# Fit model 1

M1 <- lmer (y ~ x + (1 | group))

# Example 1:  y_i ~ N (a_{group[i]} + b*x_i, sigma.y^2)
#   a_j ~ N (mu.a, sigma.a^2)
# True (population) values from simulation:
#   mu.a=5, b=-3, sigma.a=2, sigma.y=6
# After fitting the model:  estimates (+/- se's) from data:
#   mu.a=5.2(+/-1.8), b=-3.3(+/-0.7), sigma.a=3.2, sigma.y=6.3

##############################################################################

# Fit model 2

u.full <- u[group]
M2 <- lmer (y ~ x + u.full + (1 | group))

# Example 2:  y_i ~ N (a_{group[i]} + b*x_i, sigma.y^2)
#   a_j ~ N (gamma.0 + gamma.1*u, sigma.a^2)
# True (population) values from simulation:
#   gamma.0=5, gamma.1=0, b=-3, sigma.a=2, sigma.y=6
# After fitting the model:  estimates (+/- se's) from data:
#   gamma.0=1.2(+/-2.6), gamma.1=2.7(+/-1.3), b=-3.2(+/-0.7), sigma.a=2.6, sigma.y=6.3

##############################################################################

# Fit model 3
# UPDATED 7 MAR 2006 TO INCLUDE INTERACTION IN MODEL 3

M3 <- lmer (y ~ x + u.full + x:u.full + (1 + x | group))

# Example 3:  y_i ~ N (a_{group[i]} + b_{group[i]}*x_i, sigma.y^2)
#   The J pairs (a_j,b_j) follow a bivariate normal distribution
#   with mean vector (gamma.0 + gamma.1*u[j], delta.0 + delta.1*u[j])
#   and covariance matrix estimated from the data
# True (population) values from simulation:
#   gamma.0=5, gamma.1=0, delta.0=-3, delta.1=0, sigma.a=2, sigma.b=0, rho.ab=0, sigma.y=6
# After fitting the model:  estimates (+/- se's) from data:
#   gamma.0=-2.4(+/-5.2), gamma.1=5.0(+/-3.2), delta.0=-1.5(+/-2.0), delta.1=-1.1(+/-1.3), sigma.a=5.1, sigma.b=1.7, rho.ab=-0.9, sigma.y=6.1

##############################################################################

# Read in data for Example 4

y4 <- read.table ("y4.dat", header=TRUE)
y <- y4$y
x <- y4$x
group <- y4$group

##############################################################################

# Fit model 4

M4 <- glmer (y ~ x + (1 | group), family=binomial(link="logit"))

# Example 4:  Pr(y_i=1) = invlogit (a_{group[i]} + b*x_i)
#   a_j ~ N (mu.a, sigma.a^2)
# True (population) values from simulation:
#   mu.a=5, b=-3, sigma.a=2
# After fitting the model:  estimates (+/- se's) from data:
#   mu.a=6.1(+/-1.5), b=-3.2(+/-0.7), sigma.a=2.5

##############################################################################

# Read in data for Example 5

y5 <- read.table ("y5.dat", header=TRUE)
y <- y5$y
x <- y5$x
z <- y5$z
group <- y5$group

##############################################################################

# Fit model 5

log.z <- log(z)
M5 <- glmer (y ~ x + (1 | group), offset=log.z, family = quasipoisson(link = "log"))
?family
# Example 5:  y_i ~ overdispersed Poisson (z_i*exp(a_{group[i]} + b*x_i))
#   a_j ~ N (mu.a, sigma.a^2)
# True (population) values from simulation:
#   mu.a=5, b=-3, sigma.a=2
# After fitting the model:  estimates (+/- se's) from data:
#   mu.a=6.4(+/-0.5), b=-3.6(+/-0.1), sigma.a=1.7

##############################################################################

# Read in data for Example 6

y6 <- read.table ("y6.dat", header=TRUE)
y <- y6$y
state <- y6$state
occupation <- y6$occupation

##############################################################################

# Fit model 6

state.occupation <- max(occupation)*(state - 1) + occupation
M6 <- lmer (y ~ 1 + (1 | state) + (1 | occupation) + (1 | state.occupation))

# Example 6:  y_i ~ N (mu + a_{state[i]} + b_{occupation[i]} + g_{state[i],occupation[i], sigma.y^2)
#   a_j ~ N (mu.a, sigma.a^2)
#   b_j ~ N (mu.b, sigma.b^2)
#   g_j ~ N (mu.g, sigma.g^2)
# True (population) values from simulation:
#   mu.4, sigma.a=2, sigma.b=4, sigma.g=5, sigma.y=6
# After fitting the model:  estimates (+/- se's) from data:
#   mu.4.5(+/-0.9), sigma.a=2.1, sigma.b=2.3, sigma.g=5.7, sigma.y=6.2

display(M6)
