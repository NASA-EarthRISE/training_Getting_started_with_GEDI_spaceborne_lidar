# Load required libraries
library(rGEDIsimulator)
library(data.table)
library(sf)

# Define the folder path containing the .h5 files
folder_path = "C:/Users/izaya.numata/Documents/r_temp/arset_servir_2025/Field_AGB_ALS/Exercise_1/output"

# Function to extract RH metrics from an .h5 file
extract_rh_metrics <- function(h5_file) {
    cat("Extracting RH metrics from:", h5_file, "\n")
    
    # Check if the file size is 0 (corrupted)
    if (file.size(h5_file) == 0) {
        stop(paste("The file", h5_file, "is corrupted or empty (0 KB). Skipping..."))
    }
    
    # Open the .h5 file and handle errors gracefully
    gediWF <- tryCatch({
        readLevel1B(h5_file)
    }, error = function(e) {
        cat("Error reading .h5 file:", h5_file, "\nError message:", e$message, "\n")
        return(NULL)
    })
    
    # Return NULL if the .h5 file could not be read
    if (is.null(gediWF)) {
        return(NULL)
    }
    
    # Extract RH metrics safely
    metrics <- tryCatch({
        gediWFMetrics(input = gediWF, outRoot = tempfile())
    }, error = function(e) {
        cat("Error extracting metrics from:", h5_file, "\nError message:", e$message, "\n")
        return(NULL)
    })
    
    # Return NULL if metrics could not be extracted
    if (is.null(metrics)) {
        return(NULL)
    }
    
    return(metrics)
}

# Function to parse the filename and extract area_code, plot_code, plot_id, and subplot_id
parse_filename <- function(filename) {
    parts <- strsplit(gsub("_gediWF_simulation.h5$", "", basename(filename)), "_")[[1]]
    area_code <- parts[1]
    plot_code <- parts[2]
    plot_id <- paste(parts[3:(length(parts) - 1)], collapse = "_")
    subplot_num <- parts[length(parts)]
    subplot_id <- paste(plot_id, subplot_num, sep = "_")
    return(list(area_code = area_code, plot_code = plot_code, plot_id = plot_id, subplot_id = subplot_id))
}

# Function to rename RH metrics columns
rename_rh_columns <- function(rh_metrics) {
    old_names <- c("rhMax 0", "rhMax 5", "rhMax 10", "rhMax 15", "rhMax 20", "rhMax 25", "rhMax 30",
                   "rhMax 35", "rhMax 40", "rhMax 45", "rhMax 50", "rhMax 55", "rhMax 60",
                   "rhMax 65", "rhMax 70", "rhMax 75", "rhMax 80", "rhMax 85", "rhMax 90",
                   "rhMax 95", "rhMax 100")
    new_names <- paste0("RH", seq(0, 100, by = 5))
    
    setnames(rh_metrics, old_names, new_names, skip_absent = TRUE)
}

# Function to process a single .h5 file
process_single_h5_file <- function(h5_file) {
    csv_file_path <- gsub("\\.h5$", "_RH_metrics.csv", h5_file)
    
    # Skip existing files
    if (file.exists(csv_file_path)) {
        cat("Skipping already processed file:", h5_file, "\n")
        return(NULL)
    }
    
    tryCatch({
        # Extract RH metrics from the .h5 file
        rh_metrics <- extract_rh_metrics(h5_file)
        
        # Skip file if extraction failed
        if (is.null(rh_metrics)) {
            cat("Skipping file due to extraction failure:", h5_file, "\n")
            return(NULL)
        }
        
        # Parse file info for additional metadata
        file_info <- parse_filename(h5_file)
        
        # Convert to data.table and select required columns
        rh_metrics <- as.data.table(rh_metrics)
        required_columns <- c("rhMax 0", "rhMax 5", "rhMax 10", "rhMax 15", "rhMax 20", "rhMax 25", "rhMax 30",
                              "rhMax 35", "rhMax 40", "rhMax 45", "rhMax 50", "rhMax 55", "rhMax 60",
                              "rhMax 65", "rhMax 70", "rhMax 75", "rhMax 80", "rhMax 85", "rhMax 90",
                              "rhMax 95", "rhMax 100")
        
        # Check for missing columns
        missing_columns <- setdiff(required_columns, colnames(rh_metrics))
        if (length(missing_columns) > 0) {
            stop(paste("The following required columns are missing:", paste(missing_columns, collapse = ", ")))
        }
        
        # Rename columns to the new pattern
        rename_rh_columns(rh_metrics)
        
        # Add additional metadata
        rh_metrics[, file_name := basename(h5_file)]
        rh_metrics[, Area_Code := file_info$area_code]
        rh_metrics[, Plot_Code := file_info$plot_code]
        rh_metrics[, Plot_ID := file_info$plot_id]
        rh_metrics[, Subplot_ID := file_info$subplot_id]
        
        # Save the processed RH metrics as a CSV file
        fwrite(rh_metrics, csv_file_path)
        cat("Saved individual RH metrics CSV:", csv_file_path, "\n")
        
    }, error = function(e) {
        cat("Error processing file:", h5_file, "\nError message:", e$message, "\n")
    })
}

# Process all .h5 files in the given folder
process_folder <- function(folder_path) {
    h5_files <- list.files(folder_path, pattern = "\\.h5$", full.names = TRUE)
    
    # Process only those .h5 files that do not have a corresponding .csv file
    for (h5_file in h5_files) {
        # Skip files that are 0 KB (corrupted)
        if (file.size(h5_file) == 0) {
            cat("Skipping corrupted file (0 KB):", h5_file, "\n")
            next
        }
        
        # Process the .h5 file if not already processed
        process_single_h5_file(h5_file)
        
        # Clean up memory
        gc()
        Sys.sleep(1)  # Allow time to free memory
    }
    
    # Additional memory cleanup
    gc()
    Sys.sleep(2)  # Allow more time to free memory
}

# Call the process_folder function with the specified folder path
process_folder(folder_path)

cat("RH metric extraction completed.\n")
