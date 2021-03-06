# Provide access to intertemporal choice model (itchmodel) functions


# Get values from PBS environment variables
parameterization <- Sys.getenv("parameterization")
n_reps <- as.integer(Sys.getenv("n_reps"))
output_dir <- Sys.getenv("output_dir")

if (stringr::str_detect(parameterization, "date_delay")) {
  frames = c("delay", "date")
} else if (stringr::str_detect(parameterization, "defer_speedup")) {
  frames = c("neutral", "defer", "speedup")
}

# Run parameter recovery
# parameter_recovery(parameterization = parameterization,
#                    frames = frames,
#                    n_reps = n_reps,
#                    output_dir = output_dir
#                    )

print(paste0("parameterization: ", parameterization))
print(paste0("n_reps: ", n_reps))
print(paste0("output_dir: ", output_dir))
print(paste0("frames: ", frames))

