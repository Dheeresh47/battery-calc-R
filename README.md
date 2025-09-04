# Phone Battery Usage Analysis

Introduction
This project uses R to analyze and validate a phone's battery usage data. It aims to identify the biggest battery-draining apps and to account for the hidden battery consumption from the phone's operating system and background processes.

Methodology
The analysis is conducted using a custom R script that reads app usage data from a CSV file. The script performs a series of calculations to determine key metrics such as drain rate and app efficiency. A key part of the methodology is a unique approach to validate the data by explicitly calculating and labeling unaccounted-for battery usage.

Key Findings and Visualizations
The analysis provides key insights into the phone's battery behavior, as shown in the following visualizations."
Battery Usage Breakdown:  "The analysis found that only a portion of the total battery drain comes from listed apps, with a significant percentage attributed to system and unaccounted usage."
App Efficiency: "This bar chart shows the most power-hungry apps on a per-minute basis, highlighting potential targets for optimization."
Screen Time vs. Battery Usage:  "The scatter plot reveals the correlation between screen time and battery usage for each app."

 How to Run the Code
