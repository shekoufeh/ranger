rm(list=ls())
library(tidyr)
library(ranger)
library(missMDA)
library(data.table)
library("batchtools")
source("./simulations_scripts-cluster/run_all_methods_part2.R")
r <- makeRegistry(file.dir="../scratch/batchtoolsDemo")
# r <- loadRegistry(file.dir="../scratch/batchtoolsDemo")

args <- expand.grid(kk=c(3:6), 
                    missingPercentage=c(0.05,0.25,0.5,0.75,0.9),
                    missingType=c("MCAR","MAR","MNAR"),
                    repId=c(1:100))

ids <- batchMap(run_all_methods_once, args=args, reg=r)
#testJob(1,reg=r)


#submitJobs(ids, reg = r, resources = list(walltime = 300, partition="batch"))

ch <- data.frame("job.id"=1:1860, "chunk"=chunk(1:1860, n.chunks=100))
submitJobs(ch, reg = r, resources = list(walltime = 12*60*60, partition="batch"))
