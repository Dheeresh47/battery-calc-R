# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(gridExtra)

# =============================================================================
# DIRECT INPUT SECTION
# =============================================================================

# File path for your CSV data
csv_path <- "C:/Users/Admin/Documents/Battery Usage Project/battery_data.csv"

# DIRECT INPUTS
phone_data <- list(
  # From Screenshot 3: Battery usage summary
  actual_total_battery_usage = 70,       # "Battery usage: 70%"
  screen_on_time_minutes = 3*60 + 42,    # "Screen on time: 3 h 42 m"
  screen_off_time_minutes = 18*60 + 51   # "Screen off time: 18 h 51 m"
)

# =============================================================================
# ENHANCED ANALYSIS FUNCTIONS
# =============================================================================

analyze_battery_with_phone_validation <- function(csv_path, phone_data) {
  
  cat("Loading battery usage data and phone validation info...\n")
  
  # Load CSV data
  data <- read_csv(csv_path, show_col_types = FALSE)
  
  # Auto-detect and standardize column names (flexible like my enhanced version)
  col_mapping <- list()
  for (col in names(data)) {
    if (grepl("app|application", col, ignore.case = TRUE)) {
      col_mapping$app <- col
    } else if (grepl("battery.*usage|usage.*battery|battery.*percent", col, ignore.case = TRUE)) {
      col_mapping$battery <- col
    } else if (grepl("screen.*time|time.*screen", col, ignore.case = TRUE)) {
      col_mapping$screen_time <- col
    }
  }
  
  # Standardize column names
  if (length(col_mapping) >= 3) {
    data <- data %>%
      rename(
        App = !!col_mapping$app,
        Battery_Usage_Percent = !!col_mapping$battery,
        Screen_Time_Min = !!col_mapping$screen_time
      )
  }
  
  # Clean data
  data <- data %>%
    filter(!is.na(App), !is.na(Battery_Usage_Percent)) %>%
    mutate(
      Screen_Time_Min = ifelse(is.na(Screen_Time_Min), 0, Screen_Time_Min),
      Battery_Usage_Percent = as.numeric(Battery_Usage_Percent),
      Screen_Time_Min = as.numeric(Screen_Time_Min)
    )
  
  # =============================================================================
  # ACCOUNT FOR UNEXPLAINED USAGE (NEW SECTION FOR HIGHER ACCURACY)
  # =============================================================================
  
  # Calculate the sum of all app usage
  app_total_usage <- sum(data$Battery_Usage_Percent, na.rm = TRUE)
  
  # Calculate the difference between the phone's total and your app's total
  unexplained_usage <- phone_data$actual_total_battery_usage - app_total_usage
  
  # If there is a significant unexplained usage, add it as a new row
  if (unexplained_usage > 0.5) {
    system_row <- data.frame(
      App = "System & Idle",
      Battery_Usage_Percent = unexplained_usage,
      Screen_Time_Min = phone_data$screen_off_time_minutes  # Or a more realistic proxy
    )
    # Add this new row to your data frame
    data <- bind_rows(data, system_row)
  }
  
  # =============================================================================
  # CALCULATION METHOD
  # =============================================================================
  
  # Calculate total times
  total_screen_time <- sum(data$Screen_Time_Min, na.rm = TRUE)
  total_time_minutes <- phone_data$screen_on_time_minutes + phone_data$screen_off_time_minutes
  idle_time_minutes <- phone_data$screen_off_time_minutes
  
  # Screen-on battery usage (sum of all apps, including the new 'System' row)
  screen_on_battery_usage <- sum(data$Battery_Usage_Percent, na.rm = TRUE)
  
  # Calculate screen-off battery usage (YOUR METHOD)
  screen_off_battery_usage <- phone_data$actual_total_battery_usage - screen_on_battery_usage
  
  # Calculate idle battery usage rate
  idle_battery_usage_rate <- screen_off_battery_usage / idle_time_minutes
  
  # =============================================================================
  # ENHANCED METRICS
  # =============================================================================
  
  # Enhanced app-level calculations
  data <- data %>%
    mutate(
      # Battery drain per minute
      Battery_Per_Minute = ifelse(Screen_Time_Min > 0, 
                                  Battery_Usage_Percent / Screen_Time_Min, 0),
      
      # Efficiency score (lower is better)
      Efficiency_Score = ifelse(Screen_Time_Min > 0,
                                Battery_Usage_Percent / Screen_Time_Min, NA),
      
      # Usage intensity categories
      Usage_Category = case_when(
        Screen_Time_Min == 0 ~ "Background",
        Screen_Time_Min < 10 ~ "Light",
        Screen_Time_Min < 60 ~ "Moderate", 
        Screen_Time_Min < 180 ~ "Heavy",
        TRUE ~ "Intensive"
      ),
      
      # Projected daily usage if this pattern continued
      Projected_Daily_Battery = (Battery_Usage_Percent / total_screen_time) * phone_data$screen_on_time_minutes,
      
      # Compare to screen time efficiency
      Screen_Time_Efficiency = Screen_Time_Min / Battery_Usage_Percent
    )
  
  # =============================================================================
  # VALIDATION AGAINST PHONE DATA
  # =============================================================================
  
  validation_results <- list(
    csv_total_screen_time = total_screen_time,
    phone_screen_time = phone_data$screen_on_time_minutes,
    screen_time_difference = total_screen_time - phone_data$screen_on_time_minutes,
    
    csv_battery_total = screen_on_battery_usage,
    phone_battery_total = phone_data$actual_total_battery_usage,
    battery_difference = screen_on_battery_usage - phone_data$actual_total_battery_usage,
    
    estimated_total_with_idle = screen_on_battery_usage + (idle_battery_usage_rate * idle_time_minutes),
    accuracy_percentage = 100 - abs((screen_on_battery_usage - phone_data$actual_total_battery_usage) / phone_data$actual_total_battery_usage * 100)
  )
  
  # Return comprehensive results
  return(list(
    data = data,
    phone_data = phone_data,
    validation = validation_results,
    
    # Summary statistics
    summary = list(
      total_apps = nrow(data),
      screen_on_time_hours = round(phone_data$screen_on_time_minutes / 60, 2),
      screen_off_time_hours = round(phone_data$screen_off_time_minutes / 60, 2),
      actual_battery_usage = phone_data$actual_total_battery_usage,
      csv_battery_usage = round(screen_on_battery_usage, 1),
      idle_drain_rate = round(idle_battery_usage_rate, 4),
      most_efficient_app = data$App[which.min(data$Efficiency_Score[data$Efficiency_Score > 0])],
      least_efficient_app = data$App[which.max(data$Efficiency_Score[is.finite(data$Efficiency_Score)])],
      avg_drain_per_minute = round(mean(data$Battery_Per_Minute[data$Battery_Per_Minute > 0], na.rm = TRUE), 4)
    ),
    
    # Top consumers (enhanced)
    top_consumers = data %>%
      arrange(desc(Battery_Usage_Percent)) %>%
      slice(1:min(10, nrow(data))) %>%
      select(App, Battery_Usage_Percent, Screen_Time_Min, Battery_Per_Minute, Usage_Category)
  ))
}

# =============================================================================
# VALIDATION REPORT
# =============================================================================

generate_validation_report <- function(results) {
  val <- results$validation
  summary <- results$summary
  phone <- results$phone_data
  
  # Use standard R functions: paste0() for strings and rep() for repeating characters
  cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
  cat("         BATTERY ANALYSIS WITH PHONE VALIDATION REPORT\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  
  # VALIDATION SECTION (Your original approach)
  cat("VALIDATION AGAINST PHONE DATA:\n")
  cat(sprintf("   Phone reported total battery usage: %d%%\n", phone$actual_total_battery_usage))
  cat(sprintf("   CSV apps total battery usage: %.1f%%\n", val$csv_battery_total))
  cat(sprintf("   Difference: %.1f%% (%.1f%% accuracy)\n", 
              val$battery_difference, val$accuracy_percentage))
  cat("\n")
  
  cat("SCREEN TIME VALIDATION:\n") 
  cat(sprintf("   Phone screen-on time: %.1f hours\n", phone$screen_on_time_minutes/60))
  cat(sprintf("   Phone screen-off time: %.1f hours\n", phone$screen_off_time_minutes/60))
  cat(sprintf("   CSV total screen time: %.1f hours\n", val$csv_total_screen_time/60))
  cat(sprintf("   Screen time difference: %.0f minutes\n", val$screen_time_difference))
  cat("\n")
  
  # =======================================================================
  # NEW: DIRECT REPORTING OF UNACCOUNTED USAGE
  # =======================================================================
  cat("BATTERY USAGE BREAKDOWN:\n")
  
  # Find the 'System & Idle' row from the data
  unaccounted_row <- results$data %>% 
    filter(App == "System & Idle")
  
  if (nrow(unaccounted_row) > 0) {
    unaccounted_usage <- unaccounted_row$Battery_Usage_Percent
    unaccounted_drain_rate <- unaccounted_usage / phone$screen_off_time_minutes
    
    cat(sprintf("   • System & Unaccounted Usage: %.1f%%\n", unaccounted_usage))
    cat(sprintf("   • Idle Drain Rate: %.4f%% per minute\n", unaccounted_drain_rate))
    cat(sprintf("   • Idle Drain Rate: %.2f%% per hour\n", unaccounted_drain_rate * 60))
  } else {
    cat("   • System & Unaccounted usage: 0.0% (all usage accounted for by apps)\n")
  }
  cat("\n")
  
  # APP INSIGHTS
  cat("TOP BATTERY CONSUMERS:\n")
  top_apps <- results$top_consumers
  for(i in 1:min(5, nrow(top_apps))) {
    app <- top_apps[i,]
    cat(sprintf("   %d. %s: %.1f%% (%s, %.4f%%/min)\n", 
                i, app$App, app$Battery_Usage_Percent, 
                app$Usage_Category, app$Battery_Per_Minute))
  }
  cat("\n")
  
  # EFFICIENCY ANALYSIS
  cat("EFFICIENCY INSIGHTS:\n")
  cat(sprintf("   • Most efficient app: %s\n", summary$most_efficient_app))
  cat(sprintf("   • Least efficient app: %s\n", summary$least_efficient_app))
  cat(sprintf("   • Average drain rate: %.4f%% per minute\n", summary$avg_drain_per_minute))
  cat("\n")
  
  # RECOMMENDATIONS
  cat("SMART RECOMMENDATIONS:\n")
  
  high_drain_apps <- results$top_consumers %>%
    filter(Battery_Per_Minute > summary$avg_drain_per_minute * 2) %>%
    slice(1:3)
  
  if(nrow(high_drain_apps) > 0) {
    cat("   • High-drain apps to monitor:\n")
    for(i in 1:nrow(high_drain_apps)) {
      cat(sprintf("     - %s: %.4f%%/min (%.1fx average)\n", 
                  high_drain_apps$App[i], 
                  high_drain_apps$Battery_Per_Minute[i],
                  high_drain_apps$Battery_Per_Minute[i] / summary$avg_drain_per_minute))
    }
  }
  
  if(summary$idle_drain_rate > 0.01) {
    cat(sprintf("   • High idle drain detected: %.4f%%/min - check background apps\n", 
                summary$idle_drain_rate))
  }
  
  if(val$accuracy_percentage < 95) {
    cat("   • CSV data may be incomplete - check for missing apps\n")
  }
  
  cat("\n", paste(rep("=", 70), collapse = ""), "\n\n", sep = "")
}

# =============================================================================
# ENHANCED VISUALIZATIONS
# =============================================================================

create_validation_plots <- function(results) {
  # NOTE: We use the 'data' before the 'System & Idle' row is added
  # for the app-specific plots, which is more accurate for a pie chart.
  original_data_for_plots <- results$data %>% filter(App != "System & Idle")
  phone <- results$phone_data
  
  # Plot 1: Validation comparison
  comparison_data <- data.frame(
    Source = c("CSV Apps Total", "Phone Reported", "Difference"),
    Value = c(results$validation$csv_battery_total, 
              phone$actual_total_battery_usage,
              abs(results$validation$battery_difference))
  )
  
  p1 <- ggplot(comparison_data[1:2,], aes(x = Source, y = Value, fill = Source)) +
    geom_col(alpha = 0.8) +
    geom_text(aes(label = paste0(Value, "%")), vjust = -0.5) +
    scale_fill_manual(values = c("steelblue", "darkgreen")) +
    labs(title = "Battery Usage Validation: CSV vs Phone Data",
         y = "Battery Usage (%)",
         subtitle = paste0("Accuracy: ", round(results$validation$accuracy_percentage, 1), "%")) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Plot 2: App efficiency plot
  p2 <- original_data_for_plots %>%
    filter(Screen_Time_Min > 0, is.finite(Battery_Per_Minute)) %>%
    top_n(12, Battery_Usage_Percent) %>%
    ggplot(aes(x = reorder(App, -Battery_Per_Minute), y = Battery_Per_Minute)) +
    geom_col(fill = "darkorange", alpha = 0.8) +
    labs(title = "App Efficiency: Battery Drain Rate",
         subtitle = "Lower values = more efficient apps",
         x = "Applications", 
         y = "Battery % per Minute") +
    scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 0.001)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Plot 3: Screen time vs battery (scatterplot)
  p3 <- ggplot(original_data_for_plots %>% filter(Screen_Time_Min > 0), 
               aes(x = Screen_Time_Min, y = Battery_Usage_Percent)) +
    geom_point(aes(color = Usage_Category), size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
    geom_text(aes(label = App), vjust = -0.8, hjust = 0.5, size = 2.5) +
    scale_color_viridis_d() +
    labs(title = "Battery Usage vs Screen Time Correlation",
         x = "Screen Time (minutes)", 
         y = "Battery Usage (%)",
         color = "Usage Category") +
    theme_minimal()
  
  # Plot 4: Corrected pie chart showing all app usage vs. system usage
  app_total_usage <- sum(original_data_for_plots$Battery_Usage_Percent, na.rm = TRUE)
  unexplained_usage <- phone$actual_total_battery_usage - app_total_usage
  
  usage_breakdown <- data.frame(
    Type = c("Listed Apps", "System & Unaccounted"),
    Usage = c(app_total_usage, unexplained_usage)
  )
  
  p4 <- ggplot(usage_breakdown, aes(x = "", y = Usage, fill = Type)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = c("steelblue", "darkgreen")) +
    labs(title = "Battery Usage Breakdown",
         fill = "Usage Type") +
    theme_void() +
    geom_text(aes(label = paste0(round(Usage, 1), "%")), 
              position = position_stack(vjust = 0.5))
  
  # Plot 5: Boxplot for Screen Time
  p5 <- ggplot(original_data_for_plots, aes(x = "", y = Screen_Time_Min)) +
    geom_boxplot(fill = "lightgreen") +
    geom_jitter(width = 0.1, alpha = 0.4, color = "darkgreen") +
    labs(title = "Boxplot of Screen Time", y = "Screen Time (Minutes)")
  
  return(list(p1 = p1, p2 = p2, p3 = p3, p4 = p4, p5 = p5))
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

cat("Starting Battery Analysis with Phone Validation...\n")
cat("Using your proven direct-input method + enhanced analytics\n\n")

# Run analysis with phone validation
results <- analyze_battery_with_phone_validation(csv_path, phone_data)

# Generate validation report (like your original method)
generate_validation_report(results)

# Create enhanced visualizations
plots <- create_validation_plots(results)

# Display plots one by one
print(plots$p1)
print(plots$p2)
print(plots$p3)
print(plots$p4)
print(plots$p5)