#####################################
# Global Variables
SIGNIFICANT_DIGITS <- 4
SMOOTHING_FACTOR <- 2
#####################################

HRV_DIR <-"D:\\Data\\R_projects\\HRV\\"

#---------------------------------------------------------------------#
hrvtools_version <- function() {
   print("0.0.1")
}

#---------------------------------------------------------------------#

process_hrv_data <- function(directory, sig_digs=5)
{
    SIGNIFICANT_DIGITS = sig_digs
    files <- list.files(path=directory, pattern = "^2" )
    number_files  <- length(files)

    metrics_trend <- data.frame()
    for (hrv_file in files)
    {
        filename_full <- paste(directory, hrv_file, sep="")
        new_line <- derive_metrics(filename_full, sig_digs=SIGNIFICANT_DIGITS)
        metrics_trend <- rbind(metrics_trend, new_line)
    }
#XXX CV
# should be a summary-function hrv_summary(...)
# report also numerical values for last day, NNs, etc.
    cv <- vector(length=length(files))
    for(i in 1:length(files)){
        cv[i] <- calculate_coefficient_variation(metrics_trend$HRV_Ithlete, i, window=7)
    }
    metrics_trend <- cbind(metrics_trend, cv)
#XXX CV
    return(metrics_trend)
}
#data_mr = process_hrv_data(paste(HRV_DIR, "dataEliteHRV\\data_hrv_morning_readiness\\", sep = ""))
# plot_trend(data_mr)

#---------------------------------------------------------------------#

# regression test for development
regression_hrv_data <- function(dir, d_reg, sig_digs = 5)
{
    d <- process_hrv_data(dir, sig_digs) 
    return(identical(d, d_reg))
    # return(d == d_reg)
}

#d_reg <- process_hrv_data(paste(HRV_DIR, "dataEliteHRV\\data_hrv_morning_readiness\\", sep = ""))
#regression_hrv_data(paste(HRV_DIR, "dataEliteHRV\\data_hrv_morning_readiness\\", sep = ""), d_reg, 5)

#---------------------------------------------------------------------#

read_hrv_data <- function(files, dir)
{
print ("Reading HRV files:")
#print(files)
    hrv_data <- list()
    index <- 1
    for (hrv_file in files)
    {
        dat <- read.csv(paste(dir, hrv_file, sep=""), header=FALSE)
        hrv_data[[index]] <- dat
        index <- index + 1
    }
return(hrv_data)
}

#---------------------------------------------------------------------#

check_hrv_data <- function(hrv_data, rr_double_limit)
{
    for (data in hrv_data)
    {
        print(check_data_integrity(data, rr_double_limit))
    }

}
# check_hrv_data(mr_1min_data, 10)

#---------------------------------------------------------------------#

check_data_integrity <- function(hrv_data, rr_double_limit=0.075, rr_lower_limit=500, rr_upper_limit=2000)
{
# return FALSE if abnormalities are observed.

    number_low_values = length(hrv_data[hrv_data <= rr_lower_limit]) 
    number_high_values = length(hrv_data[hrv_data >= rr_upper_limit])  
    number_doubles = sum(diff(hrv_data) == 0)

    if (number_low_values + number_high_values >= 1) return(FALSE)
    if (number_doubles > rr_double_limit * length(hrv_data)) return (FALSE) # TBD: reasonable value for the number of doubles? Spread over ca. 300 ms, with ca. 150 measurements <=2? <=5.
    return(TRUE)
}
#data_mr = process_hrv_data(paste(HRV_DIR, "dataEliteHRV\\data_hrv_morning_readiness\\"))

#---------------------------------------------------------------------#

derive_metrics <- function(hrv_file, sig_digs)
{
    # set parameters
    SIGNIFICANT_DIGITS <- sig_digs
    SMOOTHING_FACTOR <- 2
    RR_ERROR_LIMIT <- 10        #values below RR_ERROR_LIMIT are considered an error message from the HR strap
    RR_LOWER_LIMIT <- 500       # no valid RR value, resting heart rate > 120 bpm
    RR_UPPER_LIMIT <- 2000      # no valid RR value, resting heart rate < 30 bpm
    RR_DOUBLE_LIMIT_PCT <- .075 # in per cent. 7.5% is a really safe margin. in practice <3% (3 doubles) are almost always the case, 5% only very seldom. 10% with the old HR strap

    # read in data 
    hrv_data <- read.csv(hrv_file, header=FALSE)

    # store raw data, preprocess, and setup data for calculations
    RR_interval <- hrv_data$V1
    RR_data_raw <- calculate_RR_data_raw(RR_interval)
    RR_data_preprocessed <- calculate_RR_data_preprocessed(RR_interval, RR_LOWER_LIMIT, RR_UPPER_LIMIT)
    RR_interval <- RR_data_preprocessed[[1]]

    HR <- 1/ RR_interval*1000*60
    RR_differences <- diff(RR_interval)
    number_intervals = length(RR_interval)

    # filename, data integrity
    filename <- strsplit(hrv_file, "\\\\")[[1]]
    file <- tail (filename, 1) 
    dataIntegrity <- check_data_integrity(RR_data_raw[[1]], rr_double_limit=RR_DOUBLE_LIMIT_PCT, 
                                          rr_lower_limit=RR_LOWER_LIMIT, rr_upper_limit=RR_UPPER_LIMIT)
    # calculate metrics
    HR_min <- calculate_HR_min(HR, SIGNIFICANT_DIGITS)
    HR_max <- calculate_HR_max(HR, SIGNIFICANT_DIGITS)
    HR_mean <- calculate_HR_mean(HR, SIGNIFICANT_DIGITS)

    HR_smoothed <- calculate_HR_smoothed(HR, SMOOTHING_FACTOR, SIGNIFICANT_DIGITS)
    HR_smoothed_min <- calculate_HR_min(HR_smoothed, SIGNIFICANT_DIGITS)
    HR_smoothed_max <- calculate_HR_max(HR_smoothed, SIGNIFICANT_DIGITS)

    RR_interval_mean <- calculate_RR_interval_mean(RR_interval, SIGNIFICANT_DIGITS)
    RMSSD <- calculate_RMSSD(RR_interval, SIGNIFICANT_DIGITS)
    NN50 <- calculate_NNx(RR_interval, 50)
    LnRMSSD <- calculate_LnRMSSD(RR_interval, SIGNIFICANT_DIGITS)
    HRV_Ithlete <- calculate_HRV_Ithlete(RR_interval, SIGNIFICANT_DIGITS)
    PNN50 <- calculate_PNNx(RR_interval, 50)
    SDNN <- calculate_SDNN(RR_interval, SIGNIFICANT_DIGITS)

    duration <- calculate_duration(RR_interval, SIGNIFICANT_DIGITS)
    duration_raw <- calculate_duration_raw(RR_data_raw, SIGNIFICANT_DIGITS)

    errors <- calculate_errors(RR_interval, SIGNIFICANT_DIGITS)
    doubles <- calculate_doubles(RR_interval, SIGNIFICANT_DIGITS)
    outlier_under <- calculate_outlier_under(RR_interval, SIGNIFICANT_DIGITS)
    outlier_over <- calculate_outlier_over(RR_interval, SIGNIFICANT_DIGITS)
 
    tmp <- 0 # use for new metric
    metrics <- data.frame(file, dataIntegrity, 
                 HR_min, HR_max, HR_mean, 
                 HR_smoothed_min, HR_smoothed_max, 
                 RR_interval_mean, RMSSD, NN50, 
                 LnRMSSD, HRV_Ithlete, PNN50, SDNN, 
                 duration, errors, doubles, outlier_under, outlier_over, tmp, stringsAsFactors=FALSE)
    metrics$RR_data_raw <- RR_data_raw
    metrics$RR_data_preprocessed <- RR_data_preprocessed
    metrics$duration_raw <- duration_raw

    return(metrics)
}
#data_mr = process_hrv_data(paste(HRV_DIR, "dataEliteHRV\\data_hrv_morning_readiness\\", sep = ""))
#data_mr$file
#regression_hrv_data(paste(HRV_DIR, "dataEliteHRV\\data_hrv_morning_readiness\\", sep = ""), d_reg)

#---------------------------------------------------------------------#
# functions to derive metrics

calculate_RR_data_raw <- function(RR_interval)
{
    RR_data_raw <- list(RR_interval)
    return(RR_data_raw)
}

calculate_RR_data_preprocessed <- function(RR_interval, rr_lower_limit=500, rr_upper_limit=2000)
{
# currently remove NAs, and outliers (rr_lower_limit, rr_upper_limit)
# TBD: remove those where relative difference to previous values is outside window (80-120%?)
    RR_data_preprocessed <- RR_interval
    RR_data_preprocessed <- RR_data_preprocessed[!is.na(RR_data_preprocessed)]
    RR_data_preprocessed <- RR_data_preprocessed[RR_data_preprocessed>=rr_lower_limit]
    RR_data_preprocessed <- RR_data_preprocessed[RR_data_preprocessed<=rr_upper_limit]
    RR_data_preprocessed <- list(RR_data_preprocessed)
    return(RR_data_preprocessed)
}
#calculate_RR_data_preprocessed(c)

calculate_HR_min <- function (HR, sig_digs=5)
{
    HR_min <- min(HR, na.rm=TRUE)
    HR_min <- signif_value(HR_min, sig_digs)
    return(HR_min)
}

calculate_HR_max <- function (HR, sig_digs=5)
{
    HR_max <- max(HR, na.rm=TRUE)
    HR_max <- signif_value(HR_max, sig_digs)
    return(HR_max)
}

calculate_HR_mean <- function (HR, sig_digs=5)
{
    HR_mean <- mean(HR, na.rm=TRUE)
    HR_mean <- signif_value(HR_mean, sig_digs)
    return(HR_mean)
}

calculate_HR_smoothed <- function (HR, smoothing_factor=2, sig_digs=5)
{
    HR_smoothed <- stats::filter(HR, rep(1/smoothing_factor, smoothing_factor), sides=1)
    HR_smoothed <- na.omit(HR_smoothed)
    HR_smoothed <- signif_value(HR_smoothed, sig_digs)
    return(HR_smoothed)
}

calculate_SDNN <- function(RR_interval, sig_digs=5)
{
    SDNN <- sd(RR_interval)
    SDNN <- signif_value(SDNN, sig_digs) 
    return(SDNN)
}

calculate_NNx <- function(RR_interval, x=50)
{
    RR_differences <- diff (RR_interval)
    NNx <- sum(RR_differences>=x) + sum(RR_differences <= (-1 * x))
    return(NNx)
}

calculate_PNNx <- function(RR_interval, x=50, sig_digs=5)
{
    NN50 <- calculate_NNx(RR_interval, x)
    number_intervals <- length(RR_interval) 
    PNNx <- NN50/(number_intervals-1)
    PNNx <- signif_value(PNNx, sig_digs)
    return(PNNx)
}

calculate_RMSSD <- function(RR_interval, sig_digs=5)
{
    RR_differences <- diff(RR_interval)
    RMSSD <- sum(RR_differences*RR_differences)/length(RR_differences)     # = HRV, alternative: RMSSD = sd(RR_differences)
    RMSSD <- sqrt(RMSSD)
    RMSSD <- signif_value(RMSSD, sig_digs)
    return(RMSSD)
}

calculate_LnRMSSD<-function(RR_interval, sig_digs=5)
{
    RMSSD <- calculate_RMSSD(RR_interval, sig_digs)
    LnRMSSD <- log(RMSSD)
    LnRMSSD <- signif_value(LnRMSSD, sig_digs)
    return(LnRMSSD)
}

calculate_RR_interval_mean <- function(RR_interval, sig_digs=5)
{
    RR_interval_mean <- mean(RR_interval)
    RR_interval_mean <- signif_value(RR_interval_mean)
    return(RR_interval_mean)
}

calculate_HRV_Ithlete <- function(RR_interval, sig_digs=5)
{
    LnRMSSD <- calculate_LnRMSSD(RR_interval, sig_digs)
    LnRMSSD <- 20 * as.numeric(LnRMSSD)
    LnRMSSD <- round_value(LnRMSSD, sig_digs)
    return(LnRMSSD)
}

calculate_duration <- function(RR_interval, sig_digs=5)
{
    duration <- sum(RR_interval) / 1000 # use for new metric
    duration <- signif_value(duration, sig_digs)
    duration <- round_value(duration)
    return(duration)
}

calculate_duration_raw <- function(RR_data_raw, sig_digs=5)
{
    duration_raw <- sum(Reduce("+",RR_data_raw))/1000
    duration_raw <- signif_value(duration_raw, sig_digs)
    duration_raw <- round_value(duration_raw)
    return(duration_raw)
}

calculate_errors <- function (RR_interval, sig_digs=5, rr_error_limit=10)
{
    errors <- sum (RR_interval <= rr_error_limit)
    return(errors)
}

calculate_doubles <- function (RR_interval, sig_digs=5)
{
    doubles <- sum (diff(RR_interval)==0)
    return(doubles)
}

calculate_outlier_under <- function(RR_interval, sig_digs=5, rr_error_limit=10, rr_lower_limit=500)
{
    RR_interval_no_errors <- RR_interval [RR_interval > 10]
    outlier_under <- sum (RR_interval_no_errors < rr_lower_limit)
    return(outlier_under)
}

calculate_outlier_over <- function (RR_interval, sig_digs=5, rr_upper_limit=2000)
{
    outlier_over <- sum(RR_interval > rr_upper_limit)
}

#---------------------------------------------------------------------#
# Summary statistics

# calculate the coefficient of variation hrv data over a parametrized window size
calculate_coefficient_variation <- function(HRV_series, ind, window=7, sig_digs=2) {
    wind <- window
    if ((ind - wind + 1) < 1) return(0)
    v <- HRV_series[(ind - wind + 1):ind]
    coefficient_variation <- sd(v) / mean(v)
    coefficient_variation <- signif_value(coefficient_variation*100, sig_digs=sig_digs)
    return (coefficient_variation)
}
# calculate_coefficient_variation(daily_hrv_scores, 120, 7, sig_digs=5)

##########################################################################################
# one function for returning / rounding to sig_digs
# maybe vector, v

signif_value <- function(raw_value, sig_digs=SIGNIFICANT_DIGITS)
{
    signif_value <- signif(raw_value, sig_digs) 
    return(signif_value)
}

round_value <- function(raw_value, sig_digs=SIGNIFICANT_DIGITS)
{
    rounded_value <- round(raw_value) 
    return(rounded_value)
}

#---------------------------------------------------------------------#
plot_trend <- function(trend_metrics, smooth=7)
{
    # TBD: find a better name for res, maybe "data_metrics"
    res <- trend_metrics
    SMOOTHING_FACTOR <- 2 #smooth # TBD: now used in two places: derive_metrics and plot_metrics.

    axis_labels <- substr(res$file, 6, 10) #cut out the date

    valid <- res$dataIntegrity == TRUE
    valid_for_plot <- valid | TRUE # TBD: later replace TRUE with " data quality sufficient function

    axis_labels <- axis_labels[valid_for_plot]

    par(mfrow=c(3,1))

# plot HR graph, showing min, mean, max
# TODO: decide on range: fixed, focus on mean? focus on most of min..max (i. e. without outliers)?
#    plot_range_HR <- as.integer(range(res$HR_min[valid_for_plot], res$HR_max[valid_for_plot], na.rm=TRUE))
#    plot_range_HR <- c(35,60)
    plot_range_HR <- as.integer(range(res$HR_min[valid_for_plot], res$HR_mean[valid_for_plot], na.rm=TRUE))

    plot_data = res$HR_min[valid_for_plot]

    plot(plot_data, type="l", col="green", main="Heart Rate", sub="blue: mean, red: max, green: min, black: moving avg", 
         xlab="file", ylab="bpm", ylim=plot_range_HR, xaxt="n")
    axis(side=1, at=1:length(axis_labels), labels=axis_labels)
    plot_data <- res$HR_max[valid_for_plot]
    lines(plot_data, col="red")
    plot_data <- res$HR_mean[valid_for_plot]
    lines(plot_data, col="blue")
    plot_data <- moving_avg(res$HR_mean[valid_for_plot], n = smooth)
    lines(plot_data, col="black", lwd=2)

    abline(h= 60, col="gray", lty=3, lwd=1)
    abline(h= 55, col="gray", lty=3, lwd=1)
    abline(h= 52.5, col="lightgray", lty=3, lwd=1)
    abline(h= 50, col="black", lty=3, lwd=1)
    abline(h= 47.5, col="lightgray", lty=3, lwd=1)
    abline(h= 45, col="gray", lty=3, lwd=1)
    abline(h= 40, col="black", lty=3, lwd=1)



# plot HRV values
    HRV_Ithlete <- 20 * as.numeric(res$LnRMSSD)
    plot_range_HRV <- as.integer(range(HRV_Ithlete[valid_for_plot], na.rm=TRUE))
    plot_data <- HRV_Ithlete[valid_for_plot]
    plot(plot_data, type="l", col="green", main="HRV", sub="green: HRV    black: HRV 7day mvg. avg.", 
         xlab="file", ylab="", ylim=plot_range_HRV, xaxt="n")

    plot_with_symbol(plot_data, !valid, 1, "red") 

    axis(side=1, at=1:length(axis_labels), labels=axis_labels)
    plot_data <- moving_avg(HRV_Ithlete[valid_for_plot], n = smooth)
    lines(plot_data, col="black", lwd=2)
    abline(h= 90, col="black", lty=3, lwd=1)
    abline(h= 85, col="lightgray", lty=3, lwd=1)
    abline(h= 80, col="gray", lty=3, lwd=1)
    abline(h= 75, col="black", lty=3, lwd=1)
    abline(h= 70, col="gray", lty=3, lwd=1)
    abline(h= 65, col="lightgray", lty=3, lwd=1)
    abline(h= 60, col="black", lty=3, lwd=1)
# plot cv values
    plot_data <- res$cv[valid_for_plot]
    plot_range_CV <- as.integer(range(res$cv[valid_for_plot], na.rm=TRUE))
    plot(plot_data, type="l", col="violet", main="HRV Coefficient of Variation", sub="violet: HRV Coefficient of Variation", 
         xlab="file", ylab="", ylim=plot_range_CV, xaxt="n")
    plot_with_symbol(plot_data, !valid, 1, "red") 
#XXX
    abline(h= 5, col="black", lty=3, lwd=1)
    abline(h= 7.5, col="grey", lty=3, lwd=1)
    abline(h= 10, col="black", lty=3, lwd=1)
    abline(h= 12.5, col="grey", lty=3, lwd=1)
    abline(h= 15, col="black", lty=3, lwd=1)
    axis(side=1, at=1:length(axis_labels), labels=axis_labels)


}
# plot_trend(data_mr)
# d<-summary_mr_trend(mr_dir, days=28)


plot_with_symbol <- function(data, factor, symbol, color) 
{
    s <- 1:length(data)
    points(s[factor], data[factor], pch=symbol, col=color)
}
# maybe with factor() setting individual colors or pchs is possible
# a<-factor(sample(1:3,20,T))
# factor(a, levels=c(TRUE, FALSE), labels=c("green", "red"))

#---------------------------------------------------------------------#
# APPLICATION
#---------------------------------------------------------------------#


######################################################################################
# Plot the morning readiness data
######################################################################################
analyse_mr <- function(mr_dir)
{
    mr_files <- list.files(path=mr_dir, pattern = "^2")
    mr_data <- read_hrv_data(mr_files, mr_dir)
    data_mr <- process_hrv_data(mr_dir)
    plot_trend(data_mr)
    print("CV - last 7")
    print(tail(data_mr$cv,7))
    return(data_mr)
}
#analyse_mr(paste(HRV_DIR, "dataEliteHRV\\data_hrv_morning_readiness\\", sep = ""))
#analyse_mr(paste(HRV_DIR, "dataEliteHRV\\data_hrv_mr_and_1min\\", sep = ""))
#analyse_mr(paste(HRV_DIR, "dataEliteHRV\\data_hrv\\", sep = ""))

####################################################################################

summary_mr_trend <- function(mr_dir, days=28, smooth=7)
{
    data_mr <- process_hrv_data(mr_dir)
    mr_files <- list.files(path=mr_dir, pattern = "^2")
    l = length(mr_files)
    k = max(1, l-days-1)
    data_last <- data_mr[k:l,]
    plot_trend(data_last, smooth) #TBD: improve, only plot relevant, print out the interesting data
    print("Last weeks cv: ")
    print(tail(data_mr$cv))
    return(data_last)
}
# mr_dir <- paste(HRV_DIR, "dataEliteHRV\\data_hrv_morning_readiness\\")
# d<-summary_mr_trend(mr_dir, 16)


#####################################################################################
# data for analysis
#####################################################################################
# mr_dir <- paste(HRV_DIR, "dataEliteHRV\\data_hrv_morning_readiness\\", sep = "")
# mr_files <- list.files(path=mr_dir, pattern = "^2")
# mr_data <- read_hrv_data(mr_files, mr_dir)
# data_mr <- process_hrv_data(mr_dir)
# plot_trend(data_mr)

#####################################################################################
# Plot Comparison of valid mr and 1_mr data
# put into a function
#####################################################################################
compare_mr_1min <- function(d)
{
    l <- length(d$HR_mean)

    par(mfrow=c(3,1))

    mr_seq <- seq(1, l, by=2)
    valid_mr_seq <- as.logical(d$dataIntegrity[mr_seq])
    valid_1min_seq <- as.logical(d$dataIntegrity[mr_seq + 1])
    valids <- valid_mr_seq & valid_1min_seq
    lim_valids <- c( min(as.numeric(d$HR_mean[as.logical(d$dataIntegrity)])), 
                     max(as.numeric(d$HR_mean[as.logical(d$dataIntegrity)]))
                    )


    axis_labels <- substr(d$file, 6, 10) #cut out the date
    axis_labels <- axis_labels[mr_seq][valids]

    plot(d$HR_mean[mr_seq][valids], col="red", type="b", pch=20, ylim=lim_valids, sub="red: mr, orange = 1min", main="HR_mean")
    axis(side=1, at=1:length(axis_labels), labels=axis_labels)
    lines(d$HR_mean[(mr_seq + 1)][valids], col="orange", pch=20)
    points(d$HR_mean[(mr_seq + 1)][valids], col="orange", pch=20)


    lim_valids <- c( min(as.numeric(d$HRV_Ithlete[as.logical(d$dataIntegrity)])), 
                     max(as.numeric(d$HRV_Ithlete[as.logical(d$dataIntegrity)]))
                    )
    plot(d$HRV_Ithlete[mr_seq][valids], col="red", type="b", pch=20, ylim=lim_valids, sub="red: mr, orange = 1min", main="HRV_Ithlete")
    axis(side=1, at=1:length(axis_labels), labels=axis_labels)
    lines(d$HRV_Ithlete[(mr_seq + 1)][valids], col="orange", pch=20)
    points(d$HRV_Ithlete[(mr_seq + 1)][valids], col="orange", pch=20)

    hrv_1min <- as.numeric(d$HRV_Ithlete[mr_seq + 1][valids])
    hrv_mr <- as.numeric(d$HRV_Ithlete[mr_seq][valids])
    hrv_diff <- hrv_1min - hrv_mr

    hr_1min <- as.numeric(d$HR_mean[mr_seq + 1][valids])
    hr_mr <- as.numeric(d$HR_mean[mr_seq][valids])
    hr_diff <- hr_1min - hr_mr

    cv_1min <- as.numeric(d$cv[mr_seq + 1][valids])
    cv_mr <- as.numeric(d$cv[mr_seq][valids])
    cv_diff <- cv_1min - cv_mr

    plot(hrv_diff, col="blue", type="l", pch=20, ylim=range(hrv_diff, na.rm=TRUE))
    title(main="Differences HR, HRV, CV", sub="blue: HRV, green: HR, violet: CV, red CV MR, orange CV 1min")
    axis(side=1, at=1:length(axis_labels), labels=axis_labels)
    lines(hr_diff, col="green", lwd=2)
    lines(cv_1min, col="orange")
    lines(cv_mr, col="red")
    lines(cv_diff, col="violet", lwd=2)
 
    abline(h= 10, col="gray", lty=3, lwd=1)
    abline(h= 5, col="lightgray", lty=3, lwd=1)
    abline(h= 0, col="black", lty=3, lwd=1)
    abline(h=-5, col="lightgray", lty=3, lwd=1)
    abline(h=-10, col="gray", lty=3, lwd=1)

    print(paste(c("HR-diff range: ", range(hr_diff, na.rm=TRUE))))
    print(paste(c("HR range: ", range(c(hr_1min, hr_mr), na.rm=TRUE))))
    print(paste(c("HRV-diff range: ", range(hrv_diff, na.rm=TRUE))))
    print(paste(c("HRV range: ", range(c(hrv_1min, hrv_mr), na.rm=TRUE))))
    print(paste(c("CV of HRV (last 7): ", tail(d$cv,7))))

}
#mr_1min_dir <- paste(HRV_DIR, "dataEliteHRV\\data_hrv_mr_and_1min\\", sep = "")
#data_mr_1min <- process_hrv_data(mr_1min_dir)
#compare_mr_1min(data_mr_1min)


#####################################################################################

compare_mr_1min_all <- function(d)
{
    l <- length(d$HR_mean)

    par(mfrow=c(3,1))

    mr_seq <- seq(1, l, by=2)

    axis_labels <- substr(d$file, 6, 10) #cut out the date
    axis_labels <- axis_labels[mr_seq]

    lim <- c( min(as.numeric(d$HR_mean)), max(as.numeric(d$HR_mean)))
    HR_mr <- d$HR_mean[mr_seq]
    HR_1min <- d$HR_mean[(mr_seq + 1)]
    plot(HR_mr, col="red", type="b", pch=20, ylim=lim, sub="red: mr, orange = 1min", main="HR_mean")
    lines(HR_1min, col="orange", pch=20)
    points(HR_1min, col="orange", pch=20)
    lines(moving_avg(HR_1min, 7), col="orange", lwd=2)
    lines(moving_avg(HR_mr, 7), col="red", lwd=2)

    lim <- c( min(as.numeric(d$HRV_Ithlete)), max(as.numeric(d$HRV_Ithlete)))
    HRV_mr <- d$HRV_Ithlete[mr_seq]
    HRV_1min <- d$HRV_Ithlete[(mr_seq + 1)]
    plot(HRV_mr, col="red", type="b", pch=20, ylim=lim, sub="red: mr, orange = 1min", main="HRV_Ithlete")
    lines(HRV_1min, col="orange", pch=20)
    points(HRV_1min, col="orange", pch=20)
    lines(moving_avg(HRV_1min, 7), col="orange", lwd=2)
    lines(moving_avg(HRV_mr, 7), col="red", lwd=2)

    hrv_1min <- as.numeric(d$HRV_Ithlete[mr_seq + 1])
    hrv_mr <- as.numeric(d$HRV_Ithlete[mr_seq])
    hrv_diff <- hrv_1min - hrv_mr

    hr_1min <- as.numeric(d$HR_mean[mr_seq + 1])
    hr_mr <- as.numeric(d$HR_mean[mr_seq])
    hr_diff <- hr_1min - hr_mr

    cv_1min <- as.numeric(d$cv[mr_seq + 1])
    cv_mr <- as.numeric(d$cv[mr_seq])
    cv_diff <- cv_1min - cv_mr

    plot(hrv_diff, col="blue", type="l", pch=20, ylim=range(hrv_diff, na.rm=TRUE))
    title(main="Differences HR, HRV, CV", sub="blue: HRV, green: HR, violet: CV, red CV MR, orange CV 1min")
    axis(side=1, at=1:length(axis_labels), labels=axis_labels)
    lines(hr_diff, col="green", lwd=2)
    lines(cv_1min, col="orange")
    lines(cv_mr, col="red")
    lines(cv_diff, col="violet", lwd=2)

    abline(h= 10, col="gray", lty=3, lwd=1)
    abline(h= 5, col="lightgray", lty=3, lwd=1)
    abline(h= 0, col="black", lty=3, lwd=1)
    abline(h=-5, col="lightgray", lty=3, lwd=1)
    abline(h=-10, col="gray", lty=3, lwd=1)

    print(paste(c("HR-diff range: ", range(hr_diff, na.rm=TRUE))))
    print(paste(c("HR range: ", range(c(hr_1min, hr_mr), na.rm=TRUE))))
    print(paste(c("HRV-diff range: ", range(hrv_diff, na.rm=TRUE))))
    print(paste(c("HRV range: ", range(c(hrv_1min, hrv_mr), na.rm=TRUE))))
    print(paste(c("CV of HRV (last 7): ", tail(d$cv,7))))

}
#mr_1min_dir <- paste(HRV_DIR, "dataEliteHRV\\data_hrv_mr_and_1min\\", sep = "")
#data_mr_1min <- process_hrv_data(mr_1min_dir)
#compare_mr_1min_all(data_mr_1min)

#####################################################################################
# read datasets: reads datasets and returns their data in one vector
#####################################################################################

read_datasets <- function (files)
{
    data <- vector(length = length(files))
    ind <- 1
    for (hrv_file in files)
    {
        data[ind] <- list(read.csv(hrv_file, header=FALSE))
        ind <- ind + 1
    }
    return(data)
}


#####################################################################################

relative_change <- function(v)
{
    v <- v[!is.na(v)]
    v1 <- v[1:(length(v)-1)]
    v2 <- v[2:length(v)]
    return (v2/v1)
}

plot_relative_change <- function(v)
{
    w <- relative_change(v)
    plot(w)
    return(w)
}
# plot_relative_change(1:100)

