corr <- function(directory, threshold = 0) {
    fileList = list.files(directory)
    vals = c()
    for(fileName in fileList)
    {
        dat <- read.csv(file = file.path(directory,fileName))
        compc <- dat[complete.cases(dat),]
        if(nrow(compc)>=threshold)
        {
            relation = cor(compc[, "sulfate"],compc[, "nitrate"])
            vals = c(vals, relation)
        }
    }
    
    vals
}

cr <- corr("specdata", 150)
head(cr)

cr <- corr("specdata", 400)
head(cr)

cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))