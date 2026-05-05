# ============================================================
# run_all.R
# Project: A perception screening framework to inform local governance of biological invasions: insights from a multisectoral community in northern Portugal
# Author: Eva Malta Pinto
#
# Purpose:
#   Run the full analytical workflow in sequence.
# ============================================================

# Run all scripts in order
source("scripts/00_setup.R")
source("scripts/01_framing_analysis.R")
source("scripts/02_species_of_concern.R")
source("scripts/03_management_acceptability.R")
source("scripts/04_expectations_analysis.R")
source("scripts/05_local_spatial_knowledge.R")

# Save session information
save_session_info()