#!/bin/bash -l
set -e
if [ "$TRIGGER_REVDEPS" ]; then
Rscript -e "rebuilds::trigger_revdeps(package='$TRIGGER_REVDEPS')"
elif [ "$SCHEDULE" = "NIGHTLY" ]; then
Rscript -e "rebuilds::trigger_all_rebuilds()"
else
echo "No command given"
exit 1
fi
echo "Action complete!"
