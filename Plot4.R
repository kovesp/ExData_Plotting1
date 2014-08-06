## Use the Electric Power Construction dataset from the UC Irvine Machine Learning Repository ...

## 4 plots in one.
##   a. Active Power
##   b. Sub Metering
##   c. Voltage
##   d. Reactive Power
# The plot is displayed and saved into a file.
#
# Parameters:
#      Parameters to override the defaults for load data, see below.
#      The data may be passed in explictly (fore ease of testing only.)
#      fn is the name of the PNG file to receive the plot
# Returns:
#      The name of the PNG file in which the plot was created
#
# The defaults for the parameters of loadData are set for ExData_Plotting1
# so normally the function can be called with no parameters.
plot4 <- function(data=NULL,fn="plot4.png",...) {
   
   if (is.null(data)) data <- loadData(4,...)
   
   doPlots4(data)
   png(filename=fn,width=480,height=480)
   doPlots4(data)
   dev.off()

   fn   
}

## Create the 4 plots. Done in a sepearate fucntion so it can be repeated
## for the screen and for PNG.
doPlots4 <- function(data) {
   par(mfcol=c(2,2))
   
   # Plot a.
   main<-""
   xlab<-""
   ylab<-"Global Active Power"
   plot(data$Time,data$Global_active_power,type="l",main=main,xlab=xlab,ylab=ylab)
   
   # PLot b.
   main<-""
   xlab<-""
   ylab<-"Energy sub metering"
   col<-c("black","red","blue")
   sel<-c("Sub_metering_1","Sub_metering_2","Sub_metering_3")
   plot(data$Time,data[[sel[1]]],type='l',col=col[1],main=main,xlab=xlab,ylab=ylab);
   lines(data$Time,data[[sel[2]]],col=col[2]);
   lines(data$Time,data[[sel[3]]],col=col[3])
   legend("topright",legend=sel,col=col,lty=1,bty="n")
   
   # Plot c.
   main<-""
   xlab<-"datetime"
   ylab<-"Voltage"
   plot(d$Time,d$Voltage,type='l',main=main,xlab=xlab,ylab=ylab)
   
   # Plot d.
   main<-""
   xlab<-"datetime"
   ylab<-"Global_reactive_power"
   plot(d$Time,d$Global_reactive_power,type='l',main=main,xlab=xlab,ylab=ylab)
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