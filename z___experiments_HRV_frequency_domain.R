#experiments hrv frequency domain measures

# vlf band: 
# lf band
# hf band

# metrics intended: hf power, lf power, hf/lf power ratio


HRV_DIR <-"D:\\Data\\R_projects\\HRV\\" 
devtools::load_all(pkg = paste(HRV_DIR, "hrvtools", sep = ""))
mr <- analyse_mr(paste(HRV_DIR, "dataEliteHRV\\data_hrv_morning_readiness\\", sep = ""))
rr_data <- mr$RR_data_raw
rr_data_pre <- mr$RR_data_preprocessed

# Data to experiment
N <- length(rr_data)
N <- 217 # Reading of 11.12.2017, 2:30 min. no artifacts 
# LF 1311.03 ms^2 (0.04 - 0.15 Hz)
# HF 902.63 ms^2 (0.15 - 0.4 Hz)
# LF/HF 1.45

VLF_ll <- 0.0001 # (or 0?)
VLF_ul <- 0.04  # Hz ~ 25.000 ms
LF_ll <- 0.04   
LF_ul <- 0.15   #  ~6666 ms
HF_ll <- 0.15   
HF_ul <- 0.4    #  ~ 2500 ms


rr <- rr_data[[N]]
rr_pre <- rr_data_pre[[N]]

rgb_set_alpha <- function(clr="red", alpha=1) {
    clr_rgb <- col2rgb(clr)
    alpha <- alpha * 255
    clr <- rgb(clr_rgb[1], clr_rgb[2], clr_rgb[3], alpha, maxColorValue=255)
    clr
}


cols = colorRampPalette(c("red", "green"))(N)

i <- 0
plot(c(0,0), ylim=c(0,100000), xlim=c(0,.5), pch=".")
for (r in rr_data) {
    i <- i + 1
    a <- pwelch(r, plot=FALSE)
    lines(a$freq, a$spec, col=rgb_set_alpha(clr=cols[i], alpha=.1))
}




plot_pwelch <- function (data, cols="red", trans=0.1, newPlot=TRUE) {
    N <- length(data)
#    cols = colorRampPalette(c("red", "green"))(N)
    if (length(cols < N)) cols = rep(cols, N)
    if (newPlot) plot(c(0,0), ylim=c(0,50000), xlim=c(0,.5), pch=".")
    i <- 0
    for (r in data) {
        i <- i + 1
        a <- pwelch(r, plot=FALSE)
        lines(a$freq, a$spec, col=rgb_set_alpha(clr=cols[i], alpha=trans))
    }
}
cols = colorRampPalette(c("red", "green"))(N)
plot_pwelch(rr_data, cols)


# plot mr vs mr_1min
par(mfrow=c(2,1))
cols="green"
plot_pwelch(rr_data[seq(1, N, by=2)], cols)
cols="red"
plot_pwelch(rr_data[seq(2, N, by=2)], cols)


# plot in 4 blocks
par(mfrow=c(1,1))
cols="green"
t <- .2
plot(c(0,0), ylim=c(0,50000), xlim=c(0,.5), pch=".")
plot_pwelch(rr_data[seq(1, N/4, by=1)], "yellow", trans=t, newPlot=FALSE)
plot_pwelch(rr_data[seq(N/4, 2*N/4, by=1)], "orange", trans=t, newPlot=FALSE)
plot_pwelch(rr_data[seq(2*N/4, 3*N/4, by=1)], "red", trans=t, newPlot=FALSE)
plot_pwelch(rr_data[seq(3*N/4, 4*N/4, by=1)], "blue", trans=t, newPlot=FALSE)




