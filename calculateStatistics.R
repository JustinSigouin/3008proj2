# don't forget to set working directory using `setwd("DIR")`
output_file <- "output.txt"; # output file for stats
data <- read.table("combined-csv-template.csv", header = TRUE, sep = ","); # read csv file, ignore the column names

# format is userId passwordScheme totalLogins successfulLogins unsuccessfulLogins avgSuccessLoginTime avgFailedLoginTime

full_data <- split(data, data$passwordScheme);

text_data = full_data$Text21;
image_data = full_data$Image21;


calculateStatistics <- function(data, scheme) {
	meanTotalUserLogins <- mean(data$totalLogins);
	cat(paste("Mean total logins for: ", scheme, meanTotalUserLogins), "\n");
}

calculateStatistics(text_data, "Text21");
calculateStatistics(image_data, "Image21");