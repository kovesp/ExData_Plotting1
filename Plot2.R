## Use the Electric Power Construction dataset from the UC Irvine Machine Learning Repository ...

## Plot Global Active power as line chart.
## The plot is displayed and saved into a file.
#
# Parameters:
#      Parameters to override the defaults for load data, see below.
#      The data may be passed in explictly (fore ease of testing only.)
# Returns:
#      The name of the PNG file in which the plot was created
#
# The defaults for the parameters of loadData are set for ExData_Plotting1
# so normally the function can be called with no parameters.
plot2 <- function(data=NULL,...) {
   
   if (is.null(data)) data <- loadData(2,...)
   
   par(mfcol=c(1,1))
   main<-""
   xlab<-""
   ylab<-"Global Active Power (kilowatts)"
   plot(data$Time,data$Global_active_power,type="l",main=main,xlab=xlab,ylab=ylab)
   
   savePlot(2)
}

## Load the data set from the text file.
#
# Parameters:
#      N an integer plot number (used only for file not found error message)
#      dir   - The directroy containing the data set
#      file  - The file name of the data set
#      dates - A vector of Date objects to select the dates in the data
#              set to use.
# Returns:
#      A data.frame containing the data for the selected dates.
#      The Date column is converted to R Date objects.
#
# The defaults are set up here to allow loadData() to be called directly. This
# allows poalying around with the data set.
#
loadData <- function(N,
                     dir   =".",
                     file  ="household_power_consumption.txt",
                     dates = c(as.Date("2007-02-01"),as.Date("2007-02-02"))) {
   fn <- paste(dir,file,sep="/")
   if (!file.exists(fn)) stop(sprintf("plot%d: File %s not found",N,fn))
   
   # Read all of the data
   data <- read.table(fn,header=TRUE,sep=";",na.strings="?")
   # Keep only the selected dates
   data <- subset(data,as.Date(Date,"%d/%m/%Y") %in% dates)
   # Replace the Time column with POSIXLt objects
   data$Time <- strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M:%S")
   # ... and drop the Date column
   data <- data[2:ncol(data)]
   
   data
}

## Save the currently displayed plot as a 480x480 pixel PNG into a file
## called 'plotN.png' and return the name of the file.
#
savePlot<-function(N) {
   fn<-sprintf("plot%d.png",N)   
   dev.copy(png,filename=fn,width=480,height=480)
   dev.off()
   fn
}