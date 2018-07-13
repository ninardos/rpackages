HRV_DIR <-"D:\\Data\\R_projects\\HRV\\" 
devtools::load_all(pkg = paste(HRV_DIR, "hrvtools", sep = ""))

library(jsonlite)

prettyJSON <- function(df)
{
  return (prettify(toJSON(df, auto_unbox=TRUE)))
}
#prettyJSON(samples)

convert_hrv_to_json <- function(directory)
{
    hrv_files <- get_hrv_files(directory)
    for (f in hrv_files)
    {
        hrv_data <- read.csv(paste(directory, hrv_file, sep=""), header=FALSE)
        data.frame(hrv_data)
# open output-file
# date and time from inputfile
# build up JSON
# RIDE
	# STARTTIME, ..., 
      # INTERVALS
      # SAMPLES
      # XDATA: 
            # HEADER, 
            # SAMPLES
# solution approach: build data frame
# write json to file
    }
}


get_hrv_files <- function(directory)
{
    files <- list.files(path=directory, pattern = "^2" )
    return(files)
}
# get_hrv_files(GC_HRV_DIR)

gen_RIDE_samples <- function(hrv_data)
{
    duration <- ceiling(sum(hrv_data)/1000)-1
    samples <- list()
    hr_average <- round(length(hrv_data[[1]]) / duration * 60)

    hr <- rr_to_hr(hrv_data[[1]])
print(length(hr))
print(duration)
    for (i in 0:duration)
    {
        samples[[i+1]] <- list( SECS=i, HR=hr[i+1], ALT=500, TEMP=20 )
#        samples[[i+1]] <- list( SECS=i, HR=hr_average, ALT=500, TEMP=20 )
    }
print(samples)
    return(samples)
}
#convert_csv_to_json(hrv_files, GC_HRV_DIR)


gen_XDATA_samples <- function(hrv_data)
{
    num_samples = length(hrv_data)
    samples <- list()
    hrv_data_secs = hrv_data/1000
    secs = 0
    for (i in 1:num_samples)
    {
        secs = secs + hrv_data_secs[i]
        samples[[i]] <- list(SECS=secs, KM=0, VALUES=list(hrv_data[i], 1))
    }
    return(samples)
}
# prettyJSON(gen_XDATA_samples(1:3))

get_starttime_from_filename <- function (hrv_file)
{
    w_day <- substr(hrv_file, 1, 10)
    w_time <- substr(hrv_file, 12, 19)
    w_day <- gsub('-', '\\/', w_day)
    w_time <- gsub('-', ':', w_time)
    starttime <- paste(w_day, w_time, "UTC", sep=" ")
    return(starttime)
}
# get_starttime_from_filename("2017-05-09 04-29-55.txt")


# redefined below in Application
GC_HRV_DIR = "D:\\R\\Dropbox\\Stefan\\Sport\\Trainingslog\\HRV\\GC_JSON\\"
HRV_DATA_DIR <- "D:\\R\\Dropbox\\Stefan\\Sport\\Trainingslog\\HRV\\neu\\"

#LAST CHANGE, marked with #CHANGED#
#CHANGED#convert_HRV_txt_to_JSON <- function (hrv_file, hrv_dir=HRV_DATA_DIR, gc_hrv_dir="D:\\R\\Dropbox\\Stefan\\Sport\\Trainingslog\\HRV\\")
convert_HRV_txt_to_JSON <- function (hrv_file, hrv_dir=HRV_DATA_DIR, gc_hrv_dir=GC_HRV_DIR)
{
    hrv_data <- read.csv(paste(hrv_dir, hrv_file, sep=""), header=FALSE)

    samples <- gen_RIDE_samples(hrv_data)
    tags <- list(Sport="Bike ", "Workout Code"="HRV_MR") #set workout code depending on file
    #intervals <- I(list(NAME="Runde 1", START=0, STOP=ceiling(sum(hrv_data)/1000 + 1)))
    hrv_samples  <- gen_XDATA_samples(hrv_data[[1]])
    xdata <- list(list(NAME="HRV", VALUES=list("R-R", "R-R flag"), UNITS=list("msecs", "bool"), SAMPLES=hrv_samples))
#    xdata <- list(NAME="HRV", VALUES=list("R-R", "R-R flag"), UNITS=list("msecs", "bool"), SAMPLES=hrv_samples)
# maybe replace I() by gen_xdata.
    starttime <- get_starttime_from_filename(hrv_file)
    ride <- list(
        STARTTIME=starttime, 
        RECINTSECS=1,
        DEVICETYPE="Polar H10 EliteHRV",
        IDENTIFIER=" ",
        TAGS=tags,
    #    INTERVALS=intervals, # maybe provide 
        SAMPLES=samples,
        XDATA=xdata
        )
    activity <- list(RIDE = ride)


    oldwd = getwd()
    setwd(GC_HRV_DIR)
    hrv_file_json <- paste(substr(hrv_file, 1,19), ".json", sep="")

    write(prettyJSON(activity), file=hrv_file_json)
    setwd(oldwd)
}


convert_csv_to_json <- function(hrv_files, gc_hrv_dir=GC_HRV_DIR)
{
    for (hrv_file in hrv_files)
	{
	    convert_HRV_txt_to_JSON(hrv_file)
	}
}


#rr_data = c(rep(1000, 30), rep(2000, 30))
#rr_data

rr_to_hr <- function(rr, window_size=3)
{
    duration <- ceiling(sum(rr)/1000)
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
print("CHANGED to cumsum, if problems occur, switch back")
    cum_rr = cumsum(rr)
#    cum_rr = vector(length=length(rr))
#    for (i in 1:length(rr))
#    {
#        cum_rr[i] = sum(rr[1:i])
#    }

    window = c(time - window_size, time + window_size)
    rr_in_window <- rr[cum_rr/1000>=window[1] & cum_rr/1000<=window[2]]

    mean_rr <- mean (rr_in_window)
    mean_hr <- round(60*1000/mean_rr)

    return(mean_hr)
}
# set_hr_from_rr (rr_data, 2, 3)
# rr_to_hr (rr_data)


# Application
GC_HRV_DIR <- "D:\\R\\Dropbox\\Stefan\\Sport\\Trainingslog\\HRV\\GC_JSON"
HRV_DATA_DIR <- "D:\\R\\Dropbox\\Stefan\\Sport\\Trainingslog\\HRV\\neu\\"
hrv_files <- get_hrv_files(HRV_DATA_DIR)
#hrv_files
convert_csv_to_json(hrv_files, GC_HRV_DIR)




#------------------------------------------------------------------------------------#
# TODO: use these functions to set hr in gc data
# enables R Plot with immediate comparison of my calculated hr and breast strap sent hr





