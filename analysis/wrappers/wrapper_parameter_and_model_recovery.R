# wrapper for parameter_and_model_recovery.Rmd

.libPaths(c("/Users/bramzandbelt/surfdrive/projects/BEWITCHING/itchmodel/packrat/lib/x86_64-apple-darwin13.4.0/3.3.3",
            "/Users/bramzandbelt/surfdrive/projects/BEWITCHING/itchmodel/packrat/lib-ext",
            "/Users/bramzandbelt/surfdrive/projects/BEWITCHING/itchmodel/packrat/lib-R")
          )

library(itchmodel)

# get script name and arguments
args <- commandArgs(TRUE)

parameterization <- as.character(args[1])
i_dataset <- as.integer(args[2])
n_repetition <- as.integer(args[3])

model_name <- "DDM"

# Specify directories
project_dir <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))
analysis_dir <- file.path(project_dir, "analysis")
notebook_reports_dir <- file.path(project_dir,"data","reports", "notebooks")
optimizations_dir <- file.path(project_dir,"data","optimizations")

notebook_name <- "parameter_and_model_recovery"
rmd_file <- file.path(analysis_dir, "parameter_and_model_recovery.Rmd")

# Verify existence of output directories
itchmodel::verify_output_dirs(base_dirs = list(optimizations_dir,
                                               notebook_reports_dir),
                              notebook_name = notebook_name)

# Specify arguments of rmarkdown::render =======================================

# Input file - an .R, .Rmd, or .md document
input <- file.path(analysis_dir, "parameter_and_model_recovery.Rmd")

# Render all format defined within the input file
output_format <- "html"

# Output file
fmt <- "parameter_and_model_recovery_model_%s_parameterization_%s_ix_%d_nrep_%d.html"
output_file <-
  sprintf(fmt = fmt,
          model_name,
          parameterization,
          i_dataset,
          n_repetition
  )

# Output directory
output_dir <- file.path(notebook_reports_dir,
                        notebook_name)

# Run rmarkdown::render ========================================================
rmarkdown::render(input = input,
                  output_file = output_file,
                  output_dir = output_dir,
                  params = list(parameterization = parameterization,
                                i_dataset = i_dataset,
                                n_repetition = n_repetition
                                )
                  )
