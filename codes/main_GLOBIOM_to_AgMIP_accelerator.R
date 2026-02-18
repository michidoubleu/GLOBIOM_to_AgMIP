#!/usr/bin/env Rscript
# ==============================================================================
# GLOBIOM to AgMIP post processing
# Author: Michael Wögerer (IIASA)
# Aim: Seamless connection of GLOBIOM to the Accelerator via routine
# ==============================================================================

# 1. Load User Settings and Arguments
source("codes/user_settings_accelerator.R")

# 2. Run the Processing Engine
source("codes/prep_GLOBIOM_agmip_accelerator.R")

