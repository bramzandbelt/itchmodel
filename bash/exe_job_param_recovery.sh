#!/bin/bash
#
# exe_job_param_recovery.sh
#
# N.B. Make sure that file's permissions allow others to execute this file
# N.B. Make sure that you update path strings and PBS .sh-file below

# Model parameterization
parameterization_string="date_delay_value_sensitivity";

# Number of repetitions
n_reps_string="90";

# Output directory
output_dir="/home/control/brazan/itchmodel/data";

# PBS Settings
WALLTIME="1:00:00";
PPN="1";
MEM="4000";

for i_parameterization in `echo $parameterization_string`
do
	for i_n_rep in `echo $n_reps_string`
	do
		qsub -l walltime=`echo $WALLTIME` -l mem=`echo "$MEM"mb` -v parameterization=`echo "$i_parameterization"`,n_rep=`echo "$i_n_rep"`,ouput_dir=`echo "$output_dir"` parameterization=`echo "$i_parameterization"` /home/control/brazan/itchmodel/pbs/pbs_job_param_recovery.sh
		# qsub -l walltime=`echo $WALLTIME` -l nodes=1:ppn=`echo $PPN` -l mem=`echo "$MEM"mb` -v parameterization=`echo "$i_parameterization"`,n_rep=`echo "$i_n_rep"`,ouput_dir=`echo "$output_dir"` parameterization=`echo "$i_parameterization"` /home/control/brazan/itchmodel/pbs/pbs_job_param_recovery.sh

	done
done
