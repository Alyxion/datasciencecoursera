fipollutantmean <- function(directory, pollutant, id = 1:332) {
    # directory: character vector of length 1 indicating the location
    # pollutant: character vector of length 1 indicating the pollutant such as sulfate or nitrate
    # id is an integer vector indicating the monitor ID numbers
    # Return: The mean of the pollutant across all monitors in the id vector
    
    value <- 0
    valueCount <- 0
    for(monitor in id)
    {
        fileName <- file.path(directory,paste(sprintf("%03d",monitor), ".csv", sep = "", collapse = ''))
        dat <- read.csv(file = fileName)
        col <- dat[pollutant]
        valid <- !is.na(col)
        valueCount <- valueCount + sum(valid)
        value <- value + sum(col[valid])
    }
    
    if(valueCount)
    {
        value / valueCount
    }
    else
    {
        0
    }
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")