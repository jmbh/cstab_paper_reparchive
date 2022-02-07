#!/bin/bash

mkdir "$TMPDIR"/clusterSim/

cd "$HOME"/clusterSim

sbatch -a 1-100 submit_jobs.sh   