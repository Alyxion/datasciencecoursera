complete <- function(directory, id = 1:332) {
    # directory defines the search directory of the file
    # id defines the id's of the files to be loaded
    # return A data frame with the columns id and nobs containing the monitor id and the count of complete cases
    
    nobs = c()
    
    for(monitor in id)
    {
        fileName <- file.path(directory,paste(sprintf("%03d",monitor), ".csv", sep = "", collapse = ''))
        dat <- read.csv(file = fileName)
        compc <- dat[complete.cases(dat),]
        rowCount <- nrow(compc)
        nobs <- c(nobs, rowCount)
    }

    res <- data.frame(id, nobs)
    res
}

complete("specdata", 1)

complete("specdata", c(2, 4, 8, 10, 12))

complete("specdata", 30:25)

complete("specdata", 3)

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])