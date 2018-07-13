# development, input to validate HRV functions, simulation of common artifacts
# if satisfying move to hrvtools
#
# function to generate a test data set and write it to disk

# TBD:
# function to generate test data (i. e. based on test vector (split/combine, where, how long, ...)


writeHRVTestdata <- function(f) {
#    old_wd <- getwd()
#    setwd("D:\\Data\\R_projects\\HRV\\data_hrv_test")
    filename <- paste("2018-01-", formatC(f, width=2, flag="0"), " 05-00-00_mr.txt", sep="")
    hr <- rnorm(150, mean=1000, sd=50)
    hr <- round(hr)
#    hr <- as.data.frame(hr)

    write.csv(hr, file=paste("D:\\Data\\R_projects\\HRV\\data_hrv_test\\", filename, ".txt"), 
              row.names = FALSE, quote = TRUE)
#    setwd(old_wd)
}

for (i in 1:10) writeHRVTestdata(i)

# function to introduce errors
# a) randomly separate a beat into two, i. e. (..., a, b, c, ...) -> (..., a, b1, b2, c, ...)
#    where b1+b2 = b (arbitrary fraction, e. g. 2/3 - 1/3
# b) randomly combine beats (..., a, b, c, ...) -> (..., a, b+c, ...)


combineRRs <- function (rr, ind) {
    rr_new <- vector(length = length(rr) - 1)
    i <- 1
    j <- 1
    while (i <= length(rr_new)) {
        if(i == ind) {
            rr_new[i] <- rr[j] + rr[j+1]
            j <- j + 1
            }
        else
            rr_new[i] <- rr[j]
        i <- i + 1
        j <- j + 1
        }
    rr_new
}
# combineRRs(1:10, 5)
# combineRRs(1:10, 1)

splitRRs <- function (rr, ind) {
    rr_new <- vector(length = length(rr) + 1)
    i <- 1
    j <- 1
    while (i <= length(rr_new)) {
        if(i == ind) {
            split = runif(1)
            rr_to_split <- rr[j]
            rr1 <- round(rr_to_split * split)
            rr2 <- rr_to_split - rr1
            rr_new[i] <-  rr1
            i <- i + 1
            rr_new[i] <- rr2
            }
        else
            rr_new[i] = rr[j]
        i <- i + 1
        j <- j + 1
        }
    rr_new
}
splitRRs(1:10, 1)



