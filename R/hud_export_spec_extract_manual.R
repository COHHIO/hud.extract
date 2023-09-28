#' @title 2024 HUD Specifications PDF
#' @export

hud_spec_2024 <- "https://files.hudexchange.info/resources/documents/HMIS-CSV-Format-Specifications-2024.pdf"

# Export.csv
# Export.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(18:20))

# reformat the third page
my_list <- cbind("", "", "", "", Export.csv[[3]])
Export.csv[[3]] <- my_list

# combine
Export <- do.call(rbind, Export.csv)

# Your matrix as a dataframe
df <- as.data.frame(your_matrix, stringsAsFactors = FALSE)

# Initialize variables
current_group <- NULL
grouped_text <- character(0)
current_value_2 <- ""
current_value_3 <- ""
current_value_4 <- ""

# Create lists to store grouped rows and associated columns 2, 3, and 4
grouped_rows <- list()
grouped_cols_2 <- list()
grouped_cols_3 <- list()
grouped_cols_4 <- list()

# Iterate through the dataframe
for (i in 1:nrow(df)) {
  # Check if the current row has a value in the first column
  if (df$V1[i] != "") {
    # If a new group is encountered, store the previous group and associated columns
    if (!is.null(current_group)) {
      grouped_rows[[current_group]] <- paste(grouped_text, collapse = "\n")
      grouped_cols_2[[current_group]] <- current_value_2
      grouped_cols_3[[current_group]] <- current_value_3
      grouped_cols_4[[current_group]] <- current_value_4
      grouped_text <- character(0)
    }
    # Set the current group and associated column values
    current_group <- df$V1[i]
    current_value_2 <- df$V2[i]
    current_value_3 <- df$V3[i]
    current_value_4 <- df$V4[i]
  }

  # Append the text in the fifth column to the current group
  if (df$V5[i] != "") {
    grouped_text <- c(grouped_text, df$V5[i])
  }
}

# Store the last group and associated columns
if (!is.null(current_group)) {
  grouped_rows[[current_group]] <- paste(grouped_text, collapse = "\n")
  grouped_cols_2[[current_group]] <- current_value_2
  grouped_cols_3[[current_group]] <- current_value_3
  grouped_cols_4[[current_group]] <- current_value_4
}

# Create a new dataframe with one row per unique value in the first column
result_df <- data.frame(
  Name = names(grouped_rows),
  V2 = unlist(grouped_cols_2),
  V3 = unlist(grouped_cols_3),
  V4 = unlist(grouped_cols_4),
  V5 = unlist(grouped_rows),
  stringsAsFactors = FALSE
)

# Remove the first row and set it as column names
colnames(result_df) <- unlist(result_df[1, ])
result_df <- result_df[-1, ]

# Print the modified dataframe
Export <- result_df


# Organization.csv
# Organization.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(20))
Organization <- as.data.frame(Organization.csv)

# Remove the first row and set it as column names
colnames(Organization) <- unlist(Organization[1, ])
Organization <- Organization[-1, ]

# User.csv
# User.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(20:21))


#### Code for dealing with 6 columns at once

process_matrix <- function(matrix_csv) {
  # Find the maximum number of columns across all matrices
  max_cols <- max(sapply(matrix_csv, ncol))

  # Pad matrices with fewer columns with NAs to match the maximum number of columns
  matrix_csv <- lapply(matrix_csv, function(mat) {
    if (ncol(mat) < max_cols) {
      diff_cols <- max_cols - ncol(mat)
      mat <- cbind(matrix("", nrow(mat), diff_cols), mat)
    }
    return(mat)
  })

  # Combine matrices in the result_list into a single matrix
  combined_matrix <- do.call(rbind, matrix_csv)

  # Your matrix as a dataframe
  df <- as.data.frame(combined_matrix, stringsAsFactors = FALSE)

  # Initialize variables
  current_group <- NULL
  grouped_text <- character(0)
  current_value_1 <- ""
  current_value_3 <- ""
  current_value_4 <- ""
  current_value_5 <- ""

  # Create lists to store grouped rows and associated columns 1, 3, 4, and 5
  grouped_rows <- list()
  grouped_cols_1 <- list()
  grouped_cols_3 <- list()
  grouped_cols_4 <- list()
  grouped_cols_5 <- list()

  # Iterate through the dataframe
  for (i in 1:nrow(df)) {
    # Check if the current row has a value in the second column
    if (df$V2[i] != "") {
      # If a new group is encountered, store the previous group and associated columns
      if (!is.null(current_group)) {
        grouped_rows[[current_group]] <- paste(grouped_text, collapse = "\n")
        grouped_cols_1[[current_group]] <- current_value_1
        grouped_cols_3[[current_group]] <- current_value_3
        grouped_cols_4[[current_group]] <- current_value_4
        grouped_cols_5[[current_group]] <- current_value_5
        grouped_text <- character(0)
      }
      # Set the current group and associated column values
      current_group <- df$V2[i]
      current_value_1 <- df$V1[i]
      current_value_3 <- df$V3[i]
      current_value_4 <- df$V4[i]
      current_value_5 <- df$V5[i]
    }

    # Append the text in the fifth column to the current group
    if (df$V6[i] != "") {
      grouped_text <- c(grouped_text, df$V6[i])
    }
  }

  # Store the last group and associated columns
  if (!is.null(current_group)) {
    grouped_rows[[current_group]] <- paste(grouped_text, collapse = "\n")
    grouped_cols_1[[current_group]] <- current_value_1
    grouped_cols_3[[current_group]] <- current_value_3
    grouped_cols_4[[current_group]] <- current_value_4
    grouped_cols_5[[current_group]] <- current_value_5
  }

  # Create a new dataframe with one row per unique value in the second column
  result_df <- data.frame(
    Name = names(grouped_rows),
    V1 = unlist(grouped_cols_1),
    V3 = unlist(grouped_cols_3),
    V4 = unlist(grouped_cols_4),
    V5 = unlist(grouped_cols_5),
    V6 = unlist(grouped_rows),
    stringsAsFactors = FALSE
  )

  # Remove the first row and set it as column names
  colnames(result_df) <- unlist(result_df[1, ])
  result_df <- result_df[-1, ]

  return(result_df)
}


# Grab table data from the rest

# Project.csv
# Project.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(21:22))

# Funder.csv
# Funder.csv <- tabulizer::extract_areas(hud_spec_2024, pages = 22)

# ProjectCoC.csv
# ProjectCoC.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(22:23))

# Inventory.csv
# Inventory.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(23:24))

# Affiliation.csv
# Affiliation.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(24))

# HMISParticipation.csv
# HMISParticipation.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(24))

# CEParticipation.csv
# CEParticipation.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(25))

# Client.csv
# Client.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(26:28))

# Enrollment.csv
# Enrollment.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(29:31))

# Exit.csv
# Exit.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(32:34))

# IncomeBenefits.csv
# IncomeBenefits.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(34:36))

# HealthAndDV.csv
# HealthAndDV.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(37))

# EmploymentEducation.csv
# EmploymentEducation.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(38))

# Disabilities.csv
# Disabilities.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(39))

# Services.csv
# Services.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(40:41))

# CurrentLivingSituation.csv
# CurrentLivingSituation.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(41:42))

# Assessment.csv
# Assessment.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(42))

# AssessmentQuestions.csv
# AssessmentQuestions.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(42:43))

# AssessmentResults.csv
# AssessmentResults.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(43))

# Events.csv
# Events.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(43:44))

# YouthEducationStatus.csv
# YouthEducationStatus.csv <- tabulizer::extract_areas(hud_spec_2024, pages = c(44))



# Create a list of the matrices
matrix_csv_list <- list(User = User.csv,
                        Project = Project.csv,
                        Funder = Funder.csv,
                        ProjectCoC = ProjectCoC.csv,
                        Inventory = Inventory.csv,
                        Affiliation = Affiliation.csv,
                        HMISParticipation = HMISParticipation.csv,
                        CEParticipation = CEParticipation.csv,
                        Client = Client.csv,
                        Enrollment = Enrollment.csv,
                        Exit = Exit.csv,
                        IncomeBenefits = IncomeBenefits.csv,
                        HealthAndDV = HealthAndDV.csv,
                        EmploymentEducation = EmploymentEducation.csv,
                        Disabilities = Disabilities.csv,
                        Services = Services.csv,
                        CurrentLivingSituation = CurrentLivingSituation.csv,
                        Assessment = Assessment.csv,
                        AssessmentQuestions = AssessmentQuestions.csv,
                        Events = Events.csv,
                        YouthEducationStatus = YouthEducationStatus.csv)

# Use purrr::map to apply the process_matrix function to each matrix
result_list <- purrr::map(matrix_csv_list, process_matrix)

# Edit AssessmentResults.csv manually
new_values <- c("4.19.6", "AssessmentResultType S250", "*", "Y", "Cannot be NULL if AssessmentResult.csv is populated")

AssessmentResults.csv[[1]][6, ] <- new_values

# Delete the specified row by creating a new matrix without it
AssessmentResult <- AssessmentResults.csv[[1]][-7, , drop = FALSE]

# Your matrix as a dataframe
df <- as.data.frame(AssessmentResult, stringsAsFactors = FALSE)

# Remove the first row and set it as column names
colnames(df) <- unlist(df[1, ])
df <- df[-1, ]

AssessmentResult.csv <- tidyr::separate(df, "Name Type", into = c("Name", "Type"), sep = " ")

# Combine Export.csv Organization.csv AssessmentResult.csv and the list of csv

csv_list <- append(result_list, list(Export = Export,
                                     Organization = Organization,
                                     AssessmentResult = AssessmentResult))

# Define the folder path where you want to save the CSV files
folder_path <- "/Users/fortyfour/Documents/COHHIO/hud.extract/inst/export_spec_tables/2024"

# Create the folder if it doesn't exist
if (!dir.exists(folder_path)) {
  dir.create(folder_path)
}

# Save each dataframe in the list as a separate CSV file
for (name in names(csv_list)) {
  file_name <- paste0(name, ".csv")  # Use the dataframe name as the file name
  file_path <- file.path(folder_path, file_name)  # Combine folder path and file name
  write.csv(csv_list[[name]], file_path, row.names = FALSE)  # Save the dataframe as CSV
}

# Print a message indicating the files have been saved
cat("CSV files saved in", folder_path, "\n")




