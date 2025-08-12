# Cuneiform Paleographic Analysis: Geometric Morphometrics Framework
# Author: [Research Team]
# Project: Measuring Scribal Literacy Through Quantitative Analysis

# ============================================================================
# PACKAGE INITIALIZATION AND DEPENDENCIES
# ============================================================================

# Core geometric morphometrics packages
library(geomorph)      # Primary geometric morphometric analysis
library(Morpho)        # Additional morphometric tools
library(shapes)        # Shape analysis and Procrustes methods

# Data manipulation and visualization
library(dplyr)         # Data manipulation
library(ggplot2)       # Advanced plotting
library(viridis)       # Color palettes for visualization
library(plotly)        # Interactive visualizations
library(corrplot)      # Correlation matrices

# Statistical analysis packages
library(vegan)         # Multivariate analysis
library(cluster)       # Clustering algorithms
library(randomForest)  # Machine learning classification
library(caret)         # Classification and regression training

# Bibliography and reproducible research
library(RefManageR)    # Bibliography management
library(knitr)         # Dynamic reporting
library(rmarkdown)     # R Markdown integration

# ============================================================================
# DATA IMPORT AND PREPROCESSING FUNCTIONS
# ============================================================================

#' Import cuneiform landmark data from standardized CSV format
#' 
#' @param file_path Character string specifying file location
#' @param coordinate_columns Vector specifying X and Y coordinate column names
#' @return Structured data frame with metadata and geometric coordinates
import_cuneiform_data <- function(file_path, coordinate_columns = c("X", "Y")) {
  
  # Validate file existence and format
  if (!file.exists(file_path)) {
    stop("Specified file does not exist: ", file_path)
  }
  
  # Import raw data with error handling
  raw_data <- tryCatch({
    read.csv(file_path, stringsAsFactors = FALSE)
  }, error = function(e) {
    stop("Failed to import data: ", e$message)
  })
  
  # Validate required columns
  required_columns <- c("SignID", "ArchiveName", "Period", coordinate_columns)
  missing_columns <- setdiff(required_columns, colnames(raw_data))
  if (length(missing_columns) > 0) {
    stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
  }
  
  # Convert coordinates to numeric and handle missing values
  raw_data[coordinate_columns] <- lapply(raw_data[coordinate_columns], as.numeric)
  
  # Create comprehensive metadata framework
  processed_data <- raw_data %>%
    mutate(
      Period_Numeric = case_when(
        Period == "OA" ~ 1,  # Old Assyrian
        Period == "MA" ~ 2,  # Middle Assyrian
        Period == "NA" ~ 3,  # Neo-Assyrian
        TRUE ~ NA_real_
      ),
      Archive_Category = case_when(
        ArchiveName %in% c("Hattusa", "Ugarit", "Alalakh", "Canaan") ~ "Peripheral",
        TRUE ~ "Mesopotamian"
      ),
      Coordinate_Quality = ifelse(
        is.na(.data[[coordinate_columns[1]]]) | is.na(.data[[coordinate_columns[2]]]), 
        "Incomplete", 
        "Complete"
      )
    )
  
  return(processed_data)
}

#' Convert landmark data to geometric morphometrics array format
#' 
#' @param data Data frame containing landmark coordinates
#' @param specimen_column Column name identifying individual specimens
#' @param landmark_column Column name identifying landmark numbers
#' @return 3D array suitable for geomorph analysis
create_landmark_array <- function(data, specimen_column = "SignID", 
                                  landmark_column = "LandmarkID") {
  
  # Validate data structure
  if (!all(c(specimen_column, landmark_column, "X", "Y") %in% colnames(data))) {
    stop("Required columns missing for array conversion")
  }
  
  # Create specimen and landmark indices
  specimens <- unique(data[[specimen_column]])
  landmarks <- unique(data[[landmark_column]])
  
  n_specimens <- length(specimens)
  n_landmarks <- length(landmarks)
  
  # Initialize 3D array: landmarks x dimensions x specimens
  landmark_array <- array(NA, dim = c(n_landmarks, 2, n_specimens))
  
  # Populate array with coordinate data
  for (i in seq_along(specimens)) {
    specimen_data <- data[data[[specimen_column]] == specimens[i], ]
    for (j in seq_along(landmarks)) {
      landmark_data <- specimen_data[specimen_data[[landmark_column]] == landmarks[j], ]
      if (nrow(landmark_data) == 1) {
        landmark_array[j, 1, i] <- landmark_data$X
        landmark_array[j, 2, i] <- landmark_data$Y
      }
    }
  }
  
  # Set array dimensions names for clarity
  dimnames(landmark_array) <- list(
    paste("Landmark", landmarks),
    c("X", "Y"),
    specimens
  )
  
  return(landmark_array)
}

# ============================================================================
# GEOMETRIC MORPHOMETRIC ANALYSIS FUNCTIONS
# ============================================================================

#' Perform Procrustes superimposition and shape analysis
#' 
#' @param landmark_array 3D array of landmark coordinates
#' @param metadata Data frame containing specimen metadata
#' @return List containing Procrustes-aligned coordinates and analysis results
procrustes_analysis <- function(landmark_array, metadata) {
  
  # Perform Generalized Procrustes Analysis
  gpa_result <- gpagen(landmark_array, print.progress = FALSE)
  
  # Extract key results
  aligned_coordinates <- gpa_result$coords
  consensus_shape <- gpa_result$consensus
  centroid_sizes <- gpa_result$Csize
  
  # Calculate shape variables for statistical analysis
  shape_variables <- two.d.array(aligned_coordinates)
  
  # Principal Component Analysis of shape variation
  pca_results <- prcomp(shape_variables, center = TRUE, scale. = FALSE)
  
  # Integrate with metadata for downstream analysis
  analysis_data <- data.frame(
    SpecimenID = dimnames(landmark_array)[[3]],
    CentroidSize = centroid_sizes,
    PC1 = pca_results$x[, 1],
    PC2 = pca_results$x[, 2],
    PC3 = pca_results$x[, 3]
  )
  
  # Merge with external metadata
  if (!is.null(metadata)) {
    analysis_data <- merge(analysis_data, metadata, by.x = "SpecimenID", by.y = "SignID")
  }
  
  return(list(
    gpa = gpa_result,
    pca = pca_results,
    analysis_data = analysis_data,
    shape_variables = shape_variables
  ))
}

#' Quantify scribal competence through morphometric deviation analysis
#' 
#' @param analysis_results Results from procrustes_analysis function
#' @param reference_group Character string identifying expert reference group
#' @return Data frame with competence scores and statistical measures
calculate_scribal_competence <- function(analysis_results, reference_group = "Mesopotamian") {
  
  data <- analysis_results$analysis_data
  shape_vars <- analysis_results$shape_variables
  
  # Identify reference specimens for baseline establishment
  reference_indices <- which(data$Archive_Category == reference_group)
  if (length(reference_indices) == 0) {
    stop("No specimens found in reference group: ", reference_group)
  }
  
  # Calculate reference centroid in shape space
  reference_centroid <- colMeans(shape_vars[reference_indices, ])
  
  # Compute Euclidean distances from reference centroid
  competence_scores <- data %>%
    mutate(
      Shape_Distance = sqrt(rowSums((shape_vars - 
                                   matrix(reference_centroid, nrow = nrow(shape_vars), 
                                          ncol = ncol(shape_vars), byrow = TRUE))^2)),
      Competence_Percentile = 100 * (1 - rank(Shape_Distance) / length(Shape_Distance)),
      Competence_Category = case_when(
        Competence_Percentile >= 75 ~ "Expert",
        Competence_Percentile >= 50 ~ "Proficient", 
        Competence_Percentile >= 25 ~ "Developing",
        TRUE ~ "Novice"
      )
    )
  
  return(competence_scores)
}

# ============================================================================
# STATISTICAL VALIDATION FUNCTIONS
# ============================================================================

#' Perform multivariate analysis of variance for group differences
#' 
#' @param shape_data Matrix of shape variables
#' @param grouping_factors Data frame of categorical variables
#' @return MANOVA results and effect sizes
validate_group_differences <- function(shape_data, grouping_factors) {
  
  # Prepare data for analysis
  combined_data <- cbind(shape_data, grouping_factors)
  
  # Multi-factor MANOVA
  if ("Archive_Category" %in% colnames(grouping_factors) && 
      "Period" %in% colnames(grouping_factors)) {
    
    manova_formula <- as.formula(paste("shape_data ~", 
                                      "Archive_Category * Period"))
    manova_results <- manova(manova_formula, data = combined_data)
    
  } else {
    stop("Required grouping factors not found")
  }
  
  # Calculate effect sizes and post-hoc comparisons
  summary_results <- summary(manova_results, test = "Pillai")
  
  return(list(
    manova = manova_results,
    summary = summary_results,
    effect_sizes = summary_results
  ))
}

#' Cross-validation assessment of classification accuracy
#' 
#' @param analysis_data Data frame with competence scores and metadata
#' @param n_folds Number of cross-validation folds
#' @return Classification accuracy metrics
cross_validate_classification <- function(analysis_data, n_folds = 10) {
  
  # Prepare data for machine learning classification
  feature_columns <- c("CentroidSize", "PC1", "PC2", "PC3", "Shape_Distance")
  features <- analysis_data[, feature_columns]
  target <- analysis_data$Archive_Category
  
  # Configure cross-validation
  train_control <- trainControl(method = "cv", number = n_folds, 
                               classProbs = TRUE, summaryFunction = multiClassSummary)
  
  # Train random forest classifier
  rf_model <- train(x = features, y = as.factor(target),
                   method = "rf", trControl = train_control,
                   metric = "Accuracy", importance = TRUE)
  
  # Extract performance metrics
  accuracy_scores <- rf_model$results
  variable_importance <- varImp(rf_model)
  
  return(list(
    model = rf_model,
    accuracy = accuracy_scores,
    importance = variable_importance,
    cv_results = rf_model$resample
  ))
}

# ============================================================================
# VISUALIZATION FUNCTIONS
# ============================================================================

#' Create comprehensive visualization suite for morphometric results
#' 
#' @param analysis_results Complete analysis results object
#' @param output_directory Directory for saving visualization files
#' @return List of ggplot objects for further customization
create_analysis_visualizations <- function(analysis_results, output_directory = "plots/") {
  
  # Ensure output directory exists
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }
  
  data <- analysis_results$analysis_data
  
  # 1. Principal Component Analysis Biplot
  pca_plot <- ggplot(data, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = Archive_Category, shape = Period), size = 3, alpha = 0.7) +
    stat_ellipse(aes(color = Archive_Category), level = 0.68, type = "norm") +
    scale_color_viridis_d(name = "Archive Type") +
    scale_shape_manual(values = c(16, 17, 18), name = "Historical Period") +
    labs(title = "Morphological Variation in Cuneiform Sign Execution",
         subtitle = "Principal Component Analysis of Landmark Configuration",
         x = paste("PC1 (", round(summary(analysis_results$pca)$importance[2,1] * 100, 1), "% variance)"),
         y = paste("PC2 (", round(summary(analysis_results$pca)$importance[2,2] * 100, 1), "% variance)")) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # 2. Scribal Competence Distribution
  competence_plot <- ggplot(data, aes(x = Archive_Category, y = Shape_Distance)) +
    geom_violin(aes(fill = Archive_Category), alpha = 0.6) +
    geom_boxplot(width = 0.1, alpha = 0.8) +
    geom_jitter(aes(color = Period), width = 0.2, alpha = 0.7) +
    scale_fill_viridis_d(name = "Archive Type") +
    scale_color_manual(values = c("#440154", "#31688E", "#35B779"), name = "Period") +
    labs(title = "Morphometric Deviation from Reference Standard",
         subtitle = "Distribution of Shape Distances Across Archive Categories",
         x = "Archive Category",
         y = "Shape Distance (Procrustes units)") +
    theme_minimal()
  
  # 3. Temporal Trends Analysis
  temporal_plot <- ggplot(data, aes(x = Period_Numeric, y = Shape_Distance)) +
    geom_smooth(aes(color = Archive_Category), method = "loess", se = TRUE) +
    geom_point(aes(color = Archive_Category), alpha = 0.6) +
    scale_color_viridis_d(name = "Archive Type") +
    scale_x_continuous(breaks = c(1, 2, 3), labels = c("Old Assyrian", "Middle Assyrian", "Neo-Assyrian")) +
    labs(title = "Diachronic Patterns in Scribal Execution Quality",
         subtitle = "Temporal Trends in Morphometric Deviation",
         x = "Historical Period",
         y = "Shape Distance from Reference") +
    theme_minimal()
  
  # Save plots to files
  ggsave(file.path(output_directory, "pca_analysis.pdf"), pca_plot, width = 10, height = 8)
  ggsave(file.path(output_directory, "competence_distribution.pdf"), competence_plot, width = 10, height = 6)
  ggsave(file.path(output_directory, "temporal_trends.pdf"), temporal_plot, width = 10, height = 6)
  
  return(list(
    pca = pca_plot,
    competence = competence_plot,
    temporal = temporal_plot
  ))
}

# ============================================================================
# MAIN ANALYSIS PIPELINE
# ============================================================================

#' Execute complete analytical pipeline
#' 
#' @param data_file Path to cuneiform landmark data file
#' @param metadata_file Path to specimen metadata file  
#' @param output_prefix Prefix for output files
#' @return Complete analysis results object
run_cuneiform_analysis <- function(data_file, metadata_file = NULL, output_prefix = "cuneiform_analysis") {
  
  cat("Initializing Cuneiform Paleographic Analysis Pipeline...\n")
  
  # 1. Data Import and Preprocessing
  cat("Step 1: Importing and preprocessing data...\n")
  raw_data <- import_cuneiform_data(data_file)
  
  if (!is.null(metadata_file)) {
    metadata <- read.csv(metadata_file, stringsAsFactors = FALSE)
  } else {
    metadata <- NULL
  }
  
  # 2. Landmark Array Creation
  cat("Step 2: Creating landmark configuration arrays...\n")
  landmark_array <- create_landmark_array(raw_data)
  
  # 3. Geometric Morphometric Analysis
  cat("Step 3: Performing Procrustes analysis and shape quantification...\n")
  morphometric_results <- procrustes_analysis(landmark_array, metadata)
  
  # 4. Scribal Competence Assessment
  cat("Step 4: Calculating scribal competence metrics...\n")
  competence_results <- calculate_scribal_competence(morphometric_results)
  
  # 5. Statistical Validation
  cat("Step 5: Validating group differences and classification accuracy...\n")
  validation_results <- validate_group_differences(
    morphometric_results$shape_variables, 
    competence_results[, c("Archive_Category", "Period")]
  )
  
  # 6. Cross-validation Assessment
  cv_results <- cross_validate_classification(competence_results)
  
  # 7. Visualization Generation
  cat("Step 6: Generating comprehensive visualizations...\n")
  morphometric_results$analysis_data <- competence_results
  visualization_results <- create_analysis_visualizations(morphometric_results)
  
  # 8. Results Integration and Export
  final_results <- list(
    data = competence_results,
    morphometrics = morphometric_results,
    validation = validation_results,
    cross_validation = cv_results,
    visualizations = visualization_results,
    metadata = list(
      analysis_date = Sys.Date(),
      data_source = data_file,
      n_specimens = nrow(competence_results),
      n_landmarks = dim(landmark_array)[1]
    )
  )
  
  # Save comprehensive results
  saveRDS(final_results, paste0(output_prefix, "_complete_results.rds"))
  write.csv(competence_results, paste0(output_prefix, "_competence_scores.csv"), row.names = FALSE)
  
  cat("Analysis pipeline completed successfully.\n")
  cat("Results saved to:", paste0(output_prefix, "_complete_results.rds"), "\n")
  
  return(final_results)
}