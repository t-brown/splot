# splot
Plot slurm account usage.

## Requirements
1. R
2. R libraries
- argeparse
- ggplot2
- reshape

## Running
Given a user or group name the script will query slurm (by `sacct`) to
obtain all jobs run  in either the last day or week. Creating a plot
of jobs/cores vs time.
