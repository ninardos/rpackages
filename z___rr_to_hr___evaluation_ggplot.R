# ------------------------------------------------------#
# new code

rr_to_hr_new <- function(rr, window_size=3)
{
    rr <- rr[!is.na(rr)]

    duration <- ceiling(sum(rr, na.rm=TRUE)/1000)
    hr <- vector(length=duration)
    cum_rr <- cumsum(rr)
    
    for (time in 1:duration)
    {
        window = c(time - window_size, time + window_size)
        rr_in_window <- rr[cum_rr/1000 >= window[1] & cum_rr/1000<=window[2]]
	mean_rr <- mean(rr_in_window)
	mean_hr <- round(60 * 1000/mean_rr)
        hr[time] <- mean_hr
    }
    return(hr)
}

# ------------------------------------------------------#
# old code

rr_to_hr <- function(rr, window_size=3)
{
    rr <- rr[!is.na(rr)]

    duration <- ceiling(sum(rr, na.rm=TRUE)/1000)
    hr <- vector(length=duration)

    for (time in 1:duration)
    {
        hr[time] <- set_hr_from_rr(rr, time, window_size)
    }

    return(hr)
}
# rr_to_hr (rr_data)

set_hr_from_rr <- function(rr, time, window_size=3)
{
    rr <- rr[!is.na(rr)]
    cum_rr = cumsum(rr)

    window = c(time - window_size, time + window_size)
    rr_in_window <- rr[cum_rr/1000>=window[1] & cum_rr/1000<=window[2]]

    mean_rr <- mean (rr_in_window)
    mean_hr <- round(60*1000/mean_rr)

    return(mean_hr)
}

GC.page(width=1500,height=1000)
par(mar=c(6,6,6,6))

WINDOWSIZE = 3
act <- GC.activity(compare=FALSE)
rrAct <- act$"HRV_R-R"

hr_own <- rr_to_hr(rrAct, window_size=WINDOWSIZE)
hr_own <- hr_own[!is.na(hr_own)]

hr_own_new <- rr_to_hr(rrAct, window_size=WINDOWSIZE)
hr_own <- hr_own_new[!is.na(hr_own_new)]

plot(act$heart.rate, col="red", type="l", lwd=3)
lines(hr_own, type="l", col="green")
lines(hr_own_new, type="l", col="lightgreen")

title("Heart rate vs. calculated heart rate")
legend("topleft", c("Heart rate", "hr_own", "hr_own_new"), fill=c("red", "green", "lightgreen"))


