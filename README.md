## AFC Calibration
This repository contains scripts and data for calibrating the Figaro Methane Sensor using reference measurements. 

## Contents
### AFC_calibration_facts.Rmd

Provides background information on the Figaro sensor calibration process.
Includes data processing steps to transform raw sensor readings into calibrated values.

### Reference Data:
LGR_May_16.csv & LGR_May_17.csv – High-precision methane reference measurements from the LGR instrument.
These datasets (already pre-processed) contain stable peak concentrations extracted after each methane injection.

### Sensor Data:
AFC_data - Raw data from the Figaro sensor, including CH₄ and CO₂ readings recorded during the calibration process.

### Peak_detection_app:
Shiny app for detecting peaks from raw LGR data

### LGR_raw_data:
Example raw LGR data

### Sensors_convert_to_ppm:
Script to convert voltage to ppm using calibration coefficients

### AFC_data_explore_app.R
Shiny app to explore raw data
