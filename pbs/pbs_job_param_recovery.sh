#!/bin/bash
#PBS -M b.zandbelt@donders.ru.nl
#PBS -o /home/control/brazan/itchmodel/pbs/output/pbs_job_optim_x_$PBS_JOBID.out
#PBS -j oe

R < /home/control/brazan/itchmodel/R/parameter_recovery_wrapper.R --no-save
