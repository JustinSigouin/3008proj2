output_file <- "output.txt";
data <- read.table("combined-csv-template.csv", header = TRUE, sep = ","); # read csv file

# format is userId passwordScheme totalLogins successfulLogins unsuccessfulLogins avgSuccessLoginTime avgFailedLoginTime

full_data <- split(data, data$passwordScheme);

text_data = full_data$Text21;
image_data = full_data$Image21;

meanTotalUserLogins <- mean(text_data$totalLogins);