#!/bin/bash
#$ -cwd -V
#$ -q long.q
#$ -M christinah.mukandavire@lshtm.ac.uk   #### USER SPECIFIC
#$ -N newmv002                   #### USER SPECIFIC
#$ -l mem_free=5G,h_vmem=5G
#$ -R y
#$ -j y
#$ -o  /home/lshcm7/clusteroe #### USER SPECIFIC
#$ -t 1-50

#### PLEASE KEEP JOB NAMES TO LENGTH 8 CHARACTERS
#### PLEASE DONT PUT DATE/TIME INFORMATION IN JOBNAMES
#### PLEASE DONT USE SPACES OR EXOTIC CHARACTERS IN JOBNAMES - ONLY ALPHANUMERIC


# Initial set up
RLD=${JOB_NAME}
ARID=${SGE_TASK_ID}

# Change working directory, create other required directories
cd "${HOME}"/L0_2020all || exit
mkdir -p "${HOME}"/console_output || exit
mkdir -p "${HOME}"/runlogs || exit

# Load R
module load R

# Select country and export as environment variable
CCODE=$(awk -v VAR="${SGE_TASK_ID}" 'BEGIN {FS=","}; NR==(VAR+1) {print $2};' ./countries/csvs/SAcomplex.csv)
export CCODE

# Capture date-time
SDT=$(date --utc +%Y-%m-%d-%H%M)

# Runlog directory and path
# A new directory with the same name as the cluster job is made to contain the runlogs
mkdir -p "${HOME}"/runlogs/"${RLD}"
RLP="${HOME}/runlogs/${RLD}/${SDT}_${RLD}_${ARID}_${CCODE}_RUNLOG.log"


# Console output directory and names
mkdir -p "${HOME}"/console_output/"${RLD}"
CON="${HOME}/console_output/${RLD}/${SDT}_${RLD}_${ARID}_${CCODE}_CO.log"

# Construct RUNID_TS
RUNID_TS="${SDT}_${RLD}_${ARID}_${CCODE}"
export RUNID_TS

# Log start
echo "${SDT} - ${RLD} - ${CCODE} - START" >> "${RLP}"

# Run Rscript; redirect stdout and stderr to CO file
Rscript AutoEmulateRerun.R -c "${CCODE}" -c ZAF -p input_L0.csv -x XMLinput_target_L0.xml -t target_L0_data.csv -n 100 >& "${CON}" || echo "$(date --utc +%Y-%m-%d-%H%M) - ${CCODE} - RSCRIPT FAILURE" >> "${RLP}"

# Signal end of script
echo "$(date --utc +%Y-%m-%d-%H%M) - ${CCODE} - END SCRIPT" >> "${RLP}"
