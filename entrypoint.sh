#!/bin/bash -l
set -e
Rscript -e "rebuilds::trigger_all_rebuilds()"
echo "Action complete!"
