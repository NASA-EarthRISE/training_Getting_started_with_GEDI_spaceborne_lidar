# Load required libraries
library(lidR)
library(rGEDIsimulator)
library(plot3D)

# Define the folder containing .las files

las_folder <- 'C:/Users/izaya.numata/Documents/r_temp/arset_servir_2025/Field_AGB_ALS/Exercise_1/plot_las'
output_folder <- 'C:/Users/izaya.numata/Documents/r_temp/arset_servir_2025/Field_AGB_ALS/Exercise_1/output01'

# Function to extract plot center geolocations
get_center_coords <- function(las) {
    xcenter <- mean(st_bbox(las)[c(1, 3)], na.rm = TRUE)
    ycenter <- mean(st_bbox(las)[c(2, 4)], na.rm = TRUE)
    return(c(xcenter, ycenter))
}

# Function to simulate GEDI waveform, save plots, and include ALS 3D scatter plot
simulate_and_plot_waveform <- function(las_file) {
    # Generate output .h5 file name
    new_file_name <- paste0(basename(tools::file_path_sans_ext(las_file)), "_gediWF_simulation.h5")
    wf_file <- file.path(output_folder, new_file_name)
    
    # Generate plot filename
    plot_file <- file.path(output_folder, paste0(basename(tools::file_path_sans_ext(las_file)), "_waveform_plot.png"))
    
    # Skip existing files
    if (file.exists(wf_file) && file.exists(plot_file)) {
        cat("Skipping existing file and plot:", wf_file, "\n")
        return(NULL)
    }
    
    tryCatch({
        # Read LAS file
        las <- readLAS(las_file)
        if (is.empty(las)) {
            cat("Empty LAS file:", las_file, "\n")
            return(NULL)
        }
        
        coords <- get_center_coords(las)
        
        # Simulate GEDI waveform
        wf <- gediWFSimulator(input = las_file, output = wf_file, coords = coords)
        
        # Generate plot for ALS data and simulated waveform
        png(plot_file, width = 8, height = 6, units = 'in', res = 300)
        par(mfrow = c(1, 2), mar = c(4, 4, 1, 1), oma = c(0, 0, 2, 2), cex.axis = 1.2)
        
        # 3D scatter plot of ALS data
        scatter3D(
            las@data$X, las@data$Y, las@data$Z,
            pch = 16, colkey = FALSE, main = "",
            cex = 0.5, bty = "u", col.panel = "gray90", phi = 30, alpha = 1, theta = 45,
            col.grid = "gray50", xlab = "UTM Easting (m)", ylab = "UTM Northing (m)", zlab = "Elevation (m)"
        )
        
        # Plot simulated GEDI waveform
        shot_number <- 0
        simulated_waveform <- getLevel1BWF(wf, shot_number)
        plot(
            simulated_waveform, relative = TRUE, polygon = TRUE, type = "l", lwd = 2, col = "forestgreen",
            xlab = "Waveform Amplitude (%)", ylab = "Elevation (m)", ylim = range(las@data$Z, na.rm = TRUE)
        )
        grid()
        
        dev.off()
        
        cat("Processed:", las_file, "\n")
    }, error = function(e) {
        cat("Error processing file:", las_file, "\nError message:", e$message, "\n")
    })
    
    # Clear memory after processing each file
    gc()
}

# Process .las files in the specified folder
process_las_files <- function(folder) {
    las_files <- list.files(folder, pattern = "\\.las$", full.names = TRUE)
    total_files <- length(las_files)
    
    if (total_files == 0) {
        cat("No .las files found in folder:", folder, "\n")
        return()
    }
    
    cat("Processing", total_files, "files in folder:", folder, "\n")
    
    for (las_file in las_files) {
        simulate_and_plot_waveform(las_file)
    }
    
    cat("Completed processing for folder:", folder, "\n")
}

# Run the simulation and plotting for the specified folder
process_las_files(las_folder)

cat("Simulation and plotting completed.\n")

