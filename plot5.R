#This function is used to answer
#How have emissions from motor vehicle sources changed from 1999–2008 in 
#Baltimore City
# Usage: 
#       plot5()
plot5 <- function(){
        
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
        
        #Get only rows for Motor Vehicle from Source_Classification_Code
        cc <- SCC[grep("*Motor*|*Vehicle*", SCC$EI.Sector,ignore.case=TRUE),]
        
        #only gather baltimore data
        bltmore <- subset(NEI, fips == "24510")
        
        # Filter all the rows, include only motor vehicle 
        mv <- bltmore[bltmore$SCC %in% cc[,1],]
        
        # Sum the emissions from all sources per year        
        pm0 <- with(mv, tapply(mv$Emissions,mv$year,sum)) 
        
        pmc <- round(pm0,digits=2)
        
        #convert array to data frame we can use the values directly by passing
        #them to as.numeric
        df <- data.frame(emission=as.numeric(pmc),year=as.numeric(names(pm0)))
                       
        #start to plot onto png device
        png("plot5.png", width=917, height=512)  
        
        #make style of axis labels horizontal
        par(mar=c(4,6,2,2),las=1)
        
        plot(df$year,df$emission,
             xlab="Year", type="b",
             ylab=expression('Total PM'[2.5]*' emission in tons'),
             main=expression('PM'[2.5]*' emission for Motor Vehicles per year for Baltimore City'),
             xaxt="n",yaxt="n")
        
        # use the values for x-axis labels 
        axis(1,at=df$year)
        
        #use the values for y-axis labels
        axis(2,at=df$emission)        
        
        #turn off the device
        dev.off()
}