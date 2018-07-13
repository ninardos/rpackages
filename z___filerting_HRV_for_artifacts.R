# test of different methods for preprocessing HRV data for artifacts. 
# If satisfying, move functions to package hrvtools

# read in some hrv-data
# summary function on quality (0, 1, -1)
# algorithm to filter out:
# everything out of window is removed recursively 
# for at most 20 times or until no further artifacts are found
# Parameters: 
#    window_length - basis for window (length of samples around current one) 
#    window_height - x% +- of mean of basis

####
####
# filter works somewhat, but problems exist for longer series and/or extreme values (might exclude even "correct" values)
# also unclear whether this can be resolved, or by what algorithm.
# more thoughts about which kinds of artifacts are likely to be there. 
# And also whether there are different kinds, and in which order to remove these.
# for filter window 2 (2 on each side) value series of x x x y y y x x can lead to deletion of
# third x (even though the ys are corrupt) if (x+y)/2 drives window so that x is outside
# typical artifacts are more like x x (x/3 x*2/3) x x 2x
# that is one instance is splitted to more than one, one is ommited and the value recorded is the sum of the ommited ones
####
####


#data for 2:30 min.
data_hrv <- round(rnorm (150, mean=1000, sd=50))
d2 <- c(rep(100, 20), rep(121,3), rep(100,20))

plot(data_hrv)

d <- data_hrv
di <- diff(d) # (d[i+1] - d[i])
r <- round(di/d [1:length(di)] * 100, 2)

# window_length: how many values to each side
remove_artifacts <- function(d, window_length, window_height)
{
    l = length(d)
    index_min = 1 + window_length
    index_max = l - window_length

    flags <- vector(length=l)
    flags[1:window_length] <- TRUE
    flags[(l-window_length):l] <- TRUE

    for (i in index_min:index_max)
    {
        w <- window(i, window_length)
        flags[i] <- is_in_window(d, i, w, window_height)
    }
    return(d[flags])
}
remove_artifacts(d, 2, .1)
d2 <- c(rep(100, 50), rep(123,10), rep(100,50)); d2
d2 <- remove_artifacts(d2, 2, .1); d2 # still 8 in
d2 <- remove_artifacts(d2, 2, .1); d2 # still 6 in


window <- function(index, window_length)
{
    wind <- c((index - window_length):(index - 1), (index + 1):(index + window_length))
    # no check on validity, e. g. window goes negative or above upper bounds
    return(wind)
}
# window(3,2)

# height as fraction, e. g. 0.2 for 20%
is_in_window <- function(data, index, wind, height)
{
    d <- data
    i <- index
    w <- wind
    m <- mean(d[w])
    h <- height

    upper_limit <-  m * (1+height)
    lower_limit <-  m / (1+height)

    data_not_below_lower_limit <- d[i] > lower_limit
    data_not_above_upper_limit  <- d[i] < upper_limit
    in_wind <- data_not_below_lower_limit & data_not_above_upper_limit

    return(in_wind)
}
# is_in_window(d, 3, w, .05)
