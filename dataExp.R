# Date Exploration with Date Explorer
# https://www.r-bloggers.com/2021/03/faster-data-exploration-with-dataexplorer/



# load DataExplorer
library(DataExplorer)

# read in dataset
diabetes_data <- read.csv("https://raw.githubusercontent.com/jbrownlee/Datasets/master/pima-indians-diabetes.csv", header = FALSE)

# fix column names
names(diabetes_data) <- c("number_of_times_pregnant", "plasma_glucose_conc", "diastolic_bp", "triceps_skinfold_thickness", "two_hr_serum_insulin", "bmi", "diabetes_pedigree_function", "age", "label")

# create report
create_report(diabetes_data)

plot_histogram(diabetes_data)

plot_bar(diabetes_data)

plot_correlation(diabetes_data)

config <- configure_report(add_plot_qq = FALSE)

create_report(config = config)

dummify(diabetes_data)
