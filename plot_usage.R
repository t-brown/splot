#!/usr/bin/env Rscript
#
# R script to plot job usage for either a user or a group.
#

library('argparse')
library('ggplot2')
library('reshape')

# Main entry point
main <- function()
{
	err <- 0

	to <- 'tbrown@example.com'
	cc <- ''
	args <- parse_args()
	if (args$group) {
		args$user <- getent_group(args$user)
	}
	dur <- duration(args$week)
	usage <- slurm_usage(args$user, dur, args$group)
	df <- map_usage(usage, dur, args$factor)
	pdf <- plot_usage(df, args$user, args$group)
	subject <- paste('\'Job usage for', args$user,
			 'from', gsub(':', '_', dur$start),
			 'till', gsub(':', '_', dur$end), '\'')
	mail(pdf, to, subject, cc=cc)
	return(err)
}

# Parse the command line arguments
parse_args <- function()
{
	# Note this is only version 1.1.1 at the time of writing.
	p <- ArgumentParser(description='Plot a user or group job usage.')
	p$add_argument('-f', '--factor', help='Accounting/core scaling factor.',
		       type='double', default=1.0)
	p$add_argument('-g', '--group', help='Plot a group instead of a user.',
		       action='store_true', default=FALSE)
	p$add_argument('-o', '--output', help='Output file name.',
		       default='jobs.pdf')
	p$add_argument('-w', '--week', help='Plot a week instead of a day.',
		       action='store_true', default=FALSE)
	p$add_argument('user', help='User name to plot.')
	args <- p$parse_args()

	return(args)
}

# Get all group members
getent_group <- function(group)
{
	everything <- system2('getent', c('group', group), stdout=TRUE)
	users <- strsplit(everything, ':')[[1]][4]
	return(users)
}

# Get the start and end times
duration <- function(week=FALSE)
{
	if (week) {
		start <- format(Sys.Date()-7,"%Y-%m-%dT00:00:00")
		dt = 86400 * 7
	} else {
		start <- format(Sys.Date()-1,"%Y-%m-%dT00:00:00")
		dt = 86400
	}
	end   <- format(Sys.Date()-1,"%Y-%m-%dT23:59:59")
	dur <- list(start=start, end=end, dt=dt)
	return(dur)
}

# Get the slurm job usage
slurm_usage <- function(users, duration, group=FALSE)
{
	fmt <- 'JobName%40,JobID,Start,End,AllocCPUS'
	if (group) {
		fmt <- 'User%40,JobID,Start,End,AllocCPUS'
	}

	dnames <- c('jobs', 'id', 'start', 'end', 'cores')
	envs <- c('TZ=UTC')
	args <- c('--delimiter=,', '-p', '-n', '--format', fmt,
		 '-S', duration$start, '-E', duration$end,
		 '--user', users)

	l <- system2('sacct', args, env=envs, stdout=TRUE)
	l <- strsplit(l, ',')
	m <- matrix(unlist(l), ncol=5, byrow=TRUE)
	m <- m[m[,1] != 'hydra_pmi_proxy',]
	m <- m[m[,1] != 'pmi_proxy',]
	m <- m[m[,1] != 'batch',]
	prev <- 0.0
	j <- 1
	q <- matrix(NA, nrow=nrow(m), ncol=5, byrow=TRUE)
	for (i in 1:nrow(m)) {
		tmp <- as.double(m[i, 2])
		if (tmp == prev) {
			q[j-1,] <- m[i,]
		} else {
			q[j,] <- m[i,]
		}
		j <- j + 1
		prev <- tmp
	}
	q <- q[!is.na(q[,1]),]
	usage <- as.data.frame(q)
	colnames(usage) <- dnames
	usage$jobs <- gsub('(\\w)_.*', '\\1', usage$jobs)
	usage$jobs <- gsub('(\\w)-.*', '\\1', usage$jobs)
	usage$start <- as.POSIXct(usage$start, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
	usage$end <- as.POSIXct(usage$end, format="%Y-%m-%dT%H:%M:%S", tz="UTC")

	return(usage)
}

# Map/convert the usage dataframe into a plotable one.
map_usage <- function(usage, duration, factor)
{
	jobs <- unique(usage$jobs)
	start <- gsub('T', ' ', duration$start)
	times <- as.POSIXct(seq(from=0, to=duration$dt, by=60), format="%Y-%m-%d %H:%M:%S",
			    origin=start, tz="UTC")

	df <- data.frame(matrix(ncol=length(jobs), nrow=length(times)))
	colnames(df) <- jobs
	df$time <- times
	sr <- round(usage$start, "mins")
	er <- round(usage$end, "mins")

	for (j in 1:nrow(usage)) {
	  cores <- usage[j, 'cores']
	  job   <- usage[j, 'jobs']
	  mins  <- seq(from=sr[j], to=er[j], by='min')
	  for (i in mins) {
	    id <- which(df$time == i)
	    df[id, paste0(job)] <- as.numeric(cores) * factor
	  }
	}

	df <- melt(df, id.vars="time")
	names(df)[names(df) == "variable"] <- "jobs"
	names(df)[names(df) == "value"] <- "cores"
	return(df)
}

plot_usage <- function(df, user, group=FALSE)
{
	if (group) {
		title <- paste('Usage for group', user)
	} else {
		title <- paste('Usage for', user)
	}
	output <- paste0(user, '.pdf')
	pdf(file=output, paper='letter')
	ggplot(df, aes(x=time, y=cores, fill=jobs, group=time)) +
	       geom_bar(stat='identity', position='stack')      +
	       labs(x="Date", y="Cores", fill="Jobs", title=title)
	invisible(dev.off())
	return(output)
}

mail <- function(output, to, subject, from=NULL, cc=NULL)
{
	if (is.null(from)) {
		reply <- to
	} else {
		reply <- from
	}
	args <- c('-s', subject, '-a', output)
	if (!is.null(cc)) {
		args <- c(args, '-c', cc)
	}
	args <- c(args, '-r', reply, to, '< /dev/null')
	#m <- system2('mailx', args, stdout=TRUE)
	print(args)
}

err <- main()
