# don't forget to set working directory using `setwd("DIR")`
output_file <- "output.pdf"; # output file for stats
data <- read.table("combined.csv", header = TRUE, sep = ","); # read csv file, ignore the column names

# format is userId passwordScheme totalLogins successfulLogins unsuccessfulLogins avgSuccessLoginTime avgFailedLoginTime

full_data <- split(data, data$passwordScheme);

text_data = full_data$Text21;
image_data = full_data$Image21;


calculateStatistics <- function(data, scheme) {
	calculateLoginStats(data, scheme);
	calculateTimeStats(data, scheme);
	pdf(file = output_file);
	createLoginHistograms(data, scheme);
	createTimeHistograms(data, scheme);
	dev.off();
}

calculateLoginStats <- function(data, scheme) {
	cat(paste("Mean for logins for", scheme),"\n");
	meanTotalUserLogins <- mean(data$totalLogins);
	cat(paste("Mean total logins for", scheme, meanTotalUserLogins), "\n");
	
	meanSuccessfulUserLogins <- mean(data$successfulLogins);
	cat(paste("Mean successful logins for", scheme, meanSuccessfulUserLogins), "\n");
	
	meanUnsuccessfulUserLogins <- mean(data$unsuccessfulLogins);
	cat(paste("Mean unsuccessful logins for", scheme, meanUnsuccessfulUserLogins), "\n");
	cat("\n");
	
	cat(paste("Standard deviation for logins for", scheme),"\n");
	sdTotalUserLogins <- sd(data$totalLogins);
	cat(paste("Standard deviation of total logins for", scheme, sdTotalUserLogins), "\n");
	
	sdSuccessfulUserLogins <- sd(data$successfulLogins);
	cat(paste("Standard deviation of successful logins for", scheme, sdSuccessfulUserLogins), "\n");
	
	sdUnsuccessfulUserLogins <- sd(data$unsuccessfulLogins);
	cat(paste("Standard deviation of unsuccessful logins for", scheme, sdUnsuccessfulUserLogins), "\n");
	cat("\n");
	
	cat(paste("Median for logins for", scheme),"\n");
	medianTotalUserLogins <- median(data$totalLogins);
	cat(paste("Median total logins for", scheme, medianTotalUserLogins), "\n");
	
	medianSuccessfulUserLogins <- median(data$successfulLogins);
	cat(paste("Median successful logins for", scheme, medianSuccessfulUserLogins), "\n");
	
	medianUnsuccessfulUserLogins <- median(data$unsuccessfulLogins);
	cat(paste("Median unsuccessful logins for", scheme, medianUnsuccessfulUserLogins), "\n");
	cat("\n");
}

calculateTimeStats <- function(data, scheme) {
	cat(paste("Mean for login time for for", scheme),"\n");
	meanSuccessfulUserLoginsTime <- mean(data$avgSuccessLoginTime);
	cat(paste("Mean successful login time for", scheme, meanSuccessfulUserLoginsTime), "\n");
	
	meanUnsuccessfulUserLoginTime <- mean(data$avgFailedLoginTime);
	cat(paste("Mean unsuccessful login time for", scheme, meanUnsuccessfulUserLoginTime), "\n");
	cat("\n");
	
	cat(paste("Standard deviation for login time for", scheme),"\n");
	sdSuccessfulUserLoginTime <- sd(data$avgSuccessLoginTime);
	cat(paste("Standard deviation of successful login time for", scheme, sdSuccessfulUserLoginTime), "\n");
	
	sdUnsuccessfulUserLoginTime <- sd(data$avgFailedLoginTime);
	cat(paste("Standard deviation of unsuccessful login time for", scheme, sdUnsuccessfulUserLoginTime), "\n");
	cat("\n");
	
	cat(paste("Median for login time for", scheme),"\n");
	medianSuccessfulUserLoginTime <- median(data$avgSuccessLoginTime);
	cat(paste("Median successful login time for", scheme, medianSuccessfulUserLoginTime), "\n");
	
	medianUnsuccessfulUserLoginTime <- median(data$avgFailedLoginTime);
	cat(paste("Median unsuccessful login time for", scheme, medianUnsuccessfulUserLoginTime), "\n");
	cat("\n");
}

createLoginHistograms <- function(data, scheme) {
	hist(data$totalLogins,main=paste(scheme, "Total Logins"),xlab="Number of Logins", col="darkmagenta");
	hist(data$successfulLogins,main=paste(scheme, "Successful Logins"),xlab="Successful Logins", col="green");
	hist(data$unsuccessfulLogins,main=paste(scheme, "Unsuccessful Logins"),xlab="Unsuccessful Logins", col="lightblue");
}

createTimeHistograms <- function(data, scheme) {
	
}

calculateStatistics(text_data, "Text21:");
calculateStatistics(image_data, "Image21:");