# Use R to set up fake data to use for the examples testing other software packages

# We create the data and save them into several files

# Set up the fake data for Examples 1-3

library(arm)

set.seed (1)
J <- 15
n <- J*(J+1)/2
group <- rep (1:J, 1:J)
mu.a <- 5
sigma.a <- 2
a <- rnorm (J, mu.a, sigma.a)
b <- -3
x <- rnorm (n, 2, 1)
sigma.y <- 6
y <- rnorm (n, a[group] + b*x, sigma.y)
u <- runif (J, 0, 3)

write.table (round (cbind (y, x, group), 2), "y123.dat", quote=FALSE, row.names=FALSE)

u.data <- cbind (1:J, u)
dimnames(u.data)[[2]] <- c("group", "u")
write.table (round (u.data, 2), "u.dat", quote=FALSE, row.names=FALSE)

# Set up the fake data for Example 4

y <- rbinom (n, 1, invlogit (a[group] + b*x))

write.table (round (cbind (y, x, group), 2), "y4.dat", quote=FALSE, row.names=FALSE)

# Set up the fake data for Example 5

z <- runif (n, 1, 10)
overdisp.noise <- exp (runif (n, 0, 2))
overdisp.noise <- overdisp.noise/mean(overdisp.noise)
y <- rpois (n, z * exp (a[group] + b*x) * overdisp.noise)

write.table (round (cbind (y, x, group, z), 2), "y5.dat", quote=FALSE, row.names=FALSE)

# Set up the fake data for Example 6

n.state <- 50
n.occupation <- 8
n.rep <- 3
n <- n.state*n.occupation*n.rep
state <- rep (1:n.state, each=n.occupation*n.rep)
occupation <- rep (1:n.occupation, n.state, each=n.rep)
state.occupation <- max(occupation)*(state - 1) + occupation
mu <- 4
sigma.state <- 2
a.state <- rnorm (n.state, 0, sigma.state)
sigma.occupation <- 4
a.occupation <- rnorm (n.occupation, 0, sigma.occupation)
sigma.state.occupation <- 5
a.state.occupation <- rnorm (n.state*n.occupation, 0, sigma.state.occupation)
sigma.y <- 6
y <- rnorm (n, mu + a.state[state] + a.occupation[occupation] + a.state.occupation[state.occupation], sigma.y)

write.table (round (cbind (y, state, occupation), 2), "y6.dat", quote=FALSE, row.names=FALSE)
