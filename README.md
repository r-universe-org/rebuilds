# Action: rebuilds

This action is invoked once per day in the [control room](https://github.com/r-universe-org/control-room) to trigger:

 1. a "retry failures" dispatch for all packages in r-universe that failed to build a source package or win/mac r-release binary in the last 5 days. The idea is that some of these failures may be temporary (e.g. a dependency wait) so we may get lucky one or two days later. We do not retry just for check failures, because this would be much more expensive and we care mostly making the packages available to users.
 2. trigger a full rebuild for packages that were uploaded exactly 30 days ago. The goal is to continuously refresh the binaries and checks within our build capacity. With 10.000 packages this amounts to 300/400 rebuilds every night, which we can do without filling the backlogs too much.
