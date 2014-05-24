#This function is used to answer
#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
#(fips == "24510") from 1999 to 2008? Use the base plotting system to make a 
#plot answering this question
# Usage: 
#       plot2()
plot2 <- function(){
        
        #filename for the zip file
        zipfileName <- "data.zip"
        
        #url to download the zip file
        fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        
        #check if files exists
        if(!file.exists("summarySCC_PM25.rds") && 
                   !file.exists("Source_Classification_Code.rds")){
                #download the file
                download.file(fileUrl,destfile=zipfileName, method="curl")
                
                #unzip the contents of zipfile
                unzip(zipfileName)
                
        }
               
        #read the data
        NEI <- readRDS("summarySCC_PM25.rds")        
        
        SCC <- readRDS("Source_Classification_Code.rds")
        
        #only gather baltimore data
        bltmore <- subset(NEI, fips == "24510")
        
        # Sum the emissions from all sources per year        
        pm0 <- with(bltmore, tapply(bltmore$Emissions,bltmore$year,sum)) 
        
        #scaling to kilo tons so the label values are easier on y-axis
        pmc <- round(pm0/1000,digits=2)
        
        #convert array to data frame we can use the values directly by passing
        #them to as.numeric
        df <- data.frame(emission=as.numeric(pmc),year=as.numeric(names(pm0)))
        
        #start to plot onto png device
        png("plot2.png", width=917, height=512)  
        
        #make style of axis labels horizontal
        par(las=1)
        
        plot(df$year,df$emission,
             xlab="Year", type="b",
             ylab=expression('Total PM'[2.5]*' emission in '~bold("kilo")~' tons'),
             main=expression('PM'[2.5]*' emission from all sources per year for Baltimore City, Maryland(fips == "24510")'),
             xaxt="n",yaxt="n")
        
        # use the values for x-axis labels 
        axis(1,at=df$year)
        
        #use the values for y-axis labels
        axis(2,at=df$emission)        
        
        #turn off the device
        dev.off()
}