devtools::load_all(pkg = "D:\\tmp\\experiments_R\\HRV\\hrvtools")
#library(hrvtools)

#####################################################################################
#####################################################################################
####                        APPLICATION
#####################################################################################
#####################################################################################
# Display results
mr <- analyse_mr("D:\\tmp\\experiments_R\\HRV\\data_hrv_morning_readiness\\")
# show data with suspicious amount of doubles
# mr <- data_mr[c("file", "doubles", "errors")][as.numeric(data_mr$doubles)>5,]


mr_1min_dir <- "D:\\tmp\\experiments_R\\HRV\\data_hrv_mr_and_1min\\"
data_mr_1min <- process_hrv_data(mr_1min_dir)
compare_mr_1min(data_mr_1min)
compare_mr_1min_all(data_mr_1min)


mr_dir <- "D:\\tmp\\experiments_R\\HRV\\data_hrv_morning_readiness\\"
#mr_dir <- "D:\\tmp\\experiments_R\\HRV\\tmp\\"
mr_sum <- summary_mr_trend(mr_dir, days=28)

test1_dir <- "D:\\tmp\\experiments_R\\HRV\\data_hrv_test\\testset1\\"
test1_sum <- summary_mr_trend(test1_dir, days=10)

test2_dir <- "D:\\tmp\\experiments_R\\HRV\\data_hrv_test\\testset2\\"
test2_sum <- summary_mr_trend(test2_dir, days=10)



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
