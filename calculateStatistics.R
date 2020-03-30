# don't forget to set working directory using `setwd("DIR")`
output_file <- "output"; # output file for stats

# format is userId passwordScheme totalLogins successfulLogins unsuccessfulLogins avgSuccessLoginTime avgFailedLoginTime

calculateStatistics <- function(data, scheme) {
	sink(paste(scheme, "text", output_file, ".txt", sep=""));
	calculateLoginStats(data, scheme);
	calculateTimeStats(data, scheme);
	sink();
	pdf(file = paste(scheme, output_file, ".pdf", sep=""));
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
	cat(paste("Mean for login time for", scheme),"\n");
	meanSuccessfulUserLoginsTime <- mean(data$avgSuccessLoginTime[data$avgSuccessLoginTime > 0]);
	cat(paste("Mean successful login time for", scheme, meanSuccessfulUserLoginsTime), "\n");
	
	meanUnsuccessfulUserLoginTime <- mean(data$avgFailedLoginTime[data$avgFailedLoginTime > 0]);
	cat(paste("Mean unsuccessful login time for", scheme, meanUnsuccessfulUserLoginTime), "\n");
	cat("\n");
	
	cat(paste("Standard deviation for login time for", scheme),"\n");
	sdSuccessfulUserLoginTime <- sd(data$avgSuccessLoginTime[data$avgSuccessLoginTime > 0]);
	cat(paste("Standard deviation of successful login time for", scheme, sdSuccessfulUserLoginTime), "\n");
	
	sdUnsuccessfulUserLoginTime <- sd(data$avgFailedLoginTime[data$avgFailedLoginTime > 0]);
	cat(paste("Standard deviation of unsuccessful login time for", scheme, sdUnsuccessfulUserLoginTime), "\n");
	cat("\n");
	
	cat(paste("Median for login time for", scheme),"\n");
	medianSuccessfulUserLoginTime <- median(data$avgSuccessLoginTime[data$avgSuccessLoginTime > 0]);
	cat(paste("Median successful login time for", scheme, medianSuccessfulUserLoginTime), "\n");
	
	medianUnsuccessfulUserLoginTime <- median(data$avgFailedLoginTime[data$avgFailedLoginTime > 0]);
	cat(paste("Median unsuccessful login time for", scheme, medianUnsuccessfulUserLoginTime), "\n");
	cat("\n");
}

createLoginHistograms <- function(data, scheme) {
	hist(data$totalLogins,main=paste(scheme, "Total Logins"), breaks=seq(0, 20, l=20), xlab="Number of Logins", col="darkmagenta");
	hist(data$successfulLogins[data$successfulLogins > 0],main=paste(scheme, "Successful Logins"), breaks=seq(0, 10, l=10), xlab="Successful Logins", col="green");
	hist(data$unsuccessfulLogins[data$unsuccessfulLogins > 0],main=paste(scheme, "Unsuccessful Logins"), breaks=seq(0, 10, l=10), xlab="Unsuccessful Logins", col="lightblue");
}

createTimeHistograms <- function(data, scheme) {
	b <- seq(0, 80, l=20);
	# ifelse(scheme == "Text21:", b <- seq(0, 20, l=20), b <- seq(0, 40, l=20));
	hist(data$avgSuccessLoginTime[data$avgSuccessLoginTime > 0], main=paste(scheme, "Average Successful Login Time"), b, xlab="Successful Login Time", col="darkmagenta");
	# ifelse(scheme == "Text21:", b <- seq(0, 30, l=30), b <- seq(0, 60, l=20));
	hist(data$avgFailedLoginTime[data$avgFailedLoginTime > 0], main=paste(scheme, "Average Unsuccessful Login Time"), b,  xlab="Unsuccessful Login Time", col="green");
	
	boxplot(data$avgSuccessLoginTime[data$avgSuccessLoginTime > 0], main=paste(scheme, "Box Plot Of Average Successful Login Time"), xlab="Successful Login Time");
	boxplot(data$avgFailedLoginTime[data$avgFailedLoginTime > 0], main=paste(scheme, "Box Plot Of Average Unsuccessful Login Time"), xlab="Unsuccessful Login Time");

}

runOtherData <- function() {
	data <- read.table("combined.csv", header = TRUE, sep = ","); # read csv file, ignore the column names
	full_data <- split(data, data$passwordScheme);

	text_data = full_data$Text21;
	image_data = full_data$Image21;
	calculateStatistics(text_data, "Text21:");
	calculateStatistics(image_data, "Image21:");
}

runOurData <- function() {	
	data <- read.table("combined_ours.csv", header = TRUE, sep = ","); # read csv file, ignore the column names
	calculateStatistics(data, "Face Pass:");
}

runText21FirstDay <- function() {	
	data <- read.table("text21_firstday.csv", header = TRUE, sep = ","); # read csv file, ignore the column names
	calculateStatistics(data, "Text21 First Day:");
}

compareSchemes <- function() {
	face_data <- read.table("combined_ours.csv", header = TRUE, sep = ","); # read csv file, ignore the column names
	
	text_data <- read.table("text21_firstday.csv", header = TRUE, sep = ","); # read csv file, ignore the column names
	result <- t.test(face_data$avgSuccessLoginTime[face_data$avgSuccessLoginTime > 0], text_data$avgSuccessLoginTime[text_data$avgSuccessLoginTime > 0], paired = FALSE);
	return(result);
}

compareSchemesUnsuccessful <- function() {
	face_data <- read.table("combined_ours.csv", header = TRUE, sep = ","); # read csv file, ignore the column names
	
	text_data <- read.table("text21_firstday.csv", header = TRUE, sep = ","); # read csv file, ignore the column names
	result <- t.test(face_data$avgFailedLoginTime[face_data$avgFailedLoginTime > 0], text_data$avgFailedLoginTime[text_data$avgFailedLoginTime > 0], paired = FALSE);
	return(result);
}

mannWhitneyTest <- function() {
	face_data <- read.table("face_sucess_fail.csv", header = TRUE, sep = ","); # read csv file, ignore the column names
	
	text_data <- read.table("text21_success_fail.csv", header = TRUE, sep = ","); # read csv file, ignore the column names
	return(wilcox.test(face_data$success, text_data$success, paired=FALSE));
}

runOtherData();
runOurData();
success <- compareSchemes();
unsucc <- compareSchemesUnsuccessful();
mann <- mannWhitneyTest();
runText21FirstDay();