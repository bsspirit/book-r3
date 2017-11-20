x <- matrix(1:(3000 * 3000), 3000, 3000)
system.time(tmp <- x %*% x)

x <- matrix(1:(3000 * 3000), 3000, 3000)
system.time(tmp <- x %*% x)

source("R-benchmark-25.R")
source("R-benchmark-25.R")

