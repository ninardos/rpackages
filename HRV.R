
HRV_DIR <-"D:\\Data\\R_projects\\HRV\\" 
devtools::load_all(pkg = paste(HRV_DIR, "hrvtools", sep = ""))

#####################################################################################
#####################################################################################
####                        APPLICATION
#####################################################################################
#####################################################################################
# Display results
mr <- analyse_mr(paste(HRV_DIR, "dataEliteHRV\\data_hrv_morning_readiness\\", sep = ""))
# show data with suspicious amount of doubles
# mr <- data_mr[c("file", "doubles", "errors")][as.numeric(data_mr$doubles)>5,]


mr_1min_dir <- paste(HRV_DIR, "dataEliteHRV\\data_hrv_mr_and_1min\\", sep = "")
data_mr_1min <- process_hrv_data(mr_1min_dir)
compare_mr_1min(data_mr_1min)
compare_mr_1min_all(data_mr_1min)


mr_dir <- paste(HRV_DIR, "dataEliteHRV\\data_hrv_morning_readiness\\", sep = "")
mr_sum <- summary_mr_trend(mr_dir, days=28, smooth=7)
#mr_sum <- summary_mr_trend(mr_dir, days=28)

test1_dir <- paste(HRV_DIR, "hrvtools\\data\\testset1\\", sep = "")
test1_sum <- summary_mr_trend(test1_dir, days=10)

test2_dir <- paste(HRV_DIR, "hrvtools\\data\\testset2\\", sep = "")
test2_sum <- summary_mr_trend(test2_dir, days=10)

####################################################################
# vgl hrv_mr und hrv_mr_1min

compare_hrv_hr_mr_1min <- function(data_dir) 
{
mr_1min_dir <- data_dir
data_mr_1min <- analyse_mr(mr_1min_dir)
N <- length(data_mr_1min$RMSSD)
ind_mr <- seq(from = 1, to = N, by = 2)
ind_1min <- seq(from = 2, to = N, by = 2)

d_mr   <- data_mr_1min$RMSSD[ind_mr]
d_1min <- data_mr_1min$RMSSD[ind_1min]
d_hr_mr <- data_mr_1min$HR_mean[ind_mr]
d_hr_1min <- data_mr_1min$HR_mean[ind_1min]

par(mfrow=c(4,1))
plot(d_mr, type="l", col="red")
lines(d_1min, col="blue")
title(paste("HRV(RMSSD)"), sub="Red: MR, Blue: 1min")
SMOOTH=7
plot(moving_avg(d_mr, n=SMOOTH), type="l", col="red")
lines(moving_avg(d_1min, n=SMOOTH), col="blue")
title(paste("Moving Average of HRV(RMSSD), n= ", SMOOTH), sub="Red: MR, Blue: 1min")

plot(d_hr_mr, type="l", col="red")
lines(d_hr_1min, col="blue")
title(paste("HR"), sub="Red: MR, Blue: 1min")
SMOOTH=7
plot(moving_avg(d_hr_mr, n=SMOOTH), type="l", col="red")
lines(moving_avg(d_hr_1min, n=SMOOTH), col="blue")
title(paste("Moving Average of HR, n= ", SMOOTH), sub="Red: MR, Blue: 1min")
}
d_dir <- paste(HRV_DIR, "dataEliteHRV\\data_hrv_mr_and_1min\\", sep = "")
compare_hrv_hr_mr_1min(d_dir)

####################################################################



#####################################################################################
# candidates for hrv_tools package

show_last_reading <- function(mr){
    par(mfrow=c(1,1))
    lastIndex <- length(mr$RR_data_raw)
    plot(unlist(mr$RR_data_raw[lastIndex]), pch=20, col="red")
    lines(unlist(mr$RR_data_preprocessed[lastIndex]), col="blue")
    title("lines: preprocessed, circles: raw")
}
show_last_reading(mr)



#
# TODO
# Export HRV from mr_reading in GC format
# https://github.com/GoldenCheetah/GoldenCheetah/wiki/HRV
# https://github.com/GoldenCheetah/GoldenCheetah/blob/master/doc/user/hrvmeasures-format.txt

#####################################################################################
# TODOS
# OPEN: Parameter um Zeitraum einzustellen, letzte x Tage z. B.
# OPEN: Achsenbeschriftung: bisher schlecht zuordenbar welche Werte von welchem Tag stammen
# OPEN: Befüllen der Datenstruktur mit numerischen Werten (anstelle Konvertieren, bei Verwendung)
# OPEN: Datenbereinigung: Ausreisser -> Kriterien hierfür noch zu erarbeiten (RMSSD > 100?, HR_min, HR_max?)
#       Auf Dateneinlese-ebene: Werte > 2000 deuten auf verlorene Signale hin. Werte < 10 deuten auf Fehlercodes hin. 
#       Werte im Bereich von HR=30 bis HR=90 sollten auch mit Varianz nicht verlassen werden, das entspricht dann 666 ms - 2000 ms
#       Notiert könnte werden: wieviele "Zeilen wurden aussortiert weil < 666 oder > 2000 ms"
#       Weitere Auffälligkeit: doppelte
# OPEN: Datenstruktur erweitern. RR teil des dataframes, einmal RR_raw und RR_processed 
#       (um Fehlercodes und unplausible Werte bereinigt)
# OPEN: refactor adding timescale (should be done in a separate function)
# OPEN: add a simple function to access data of a particular day
# OPEN: add preprocessing, data filtering (exclude values in calculations that are 
#       not within 20% of "current window -> how to define window exactly
#       and that are outside of hard limits
#       in graphs / metrics give feedback on problems found, such as % of excluded values (ranges) , hard limits not kept, ...
# OPEN: in plotting trends: invalid data symbol according to problems identified in data set
# OPEN - data integrity check: for mr-1min if pairs are as expected, based on file name (same date, ending  _mr.txt, _mr_1min.txt, few minutes later, ...)
# OPEN - unify mr-1min and mr-1min_all: plot all, mark invalids by circle or similar
# OPEN - include HR and HRV histogram, include HR-delta, HRV-delta histogramms e. g. hist(DATAXXX, breaks = seq(min-.5, max+.5, 1))
# OPEN - EXPERIMENT: analyse impact of setting limits to 75 %, 80 %, 85 % and "window size" from 1 (previous value) to 10 (previous 10 values)
#                        what does literature recommend? 1?, 3?, 5? -> shorter is able to sort out fewer damples
