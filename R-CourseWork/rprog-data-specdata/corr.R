corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations

files <- list.files(directory)

	corr_vector <- rep(NaN,length(files))
	counter <- 1
	for (file_name in files)
	{

		data <- read.csv(paste(directory,"/",file_name, sep=""))

		data <- data[complete.cases(data),]
	
		if (nrow(data) <= threshold)
		{
			next
		}

		nitrate<- data[,"nitrate"]
		sulfate<- data[,"sulfate"]
		corr_vector[counter] <- cor(nitrate,sulfate)
		counter <- counter +1
	}
	corr_vector[!is.na(corr_vector)]
}