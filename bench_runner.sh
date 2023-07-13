#!/usr/bin/env sh

if [ $1 == "-h" ] || [ $1 == "--help" ] ; then
    echo "USAGE: RUNS=<runs> ./bench_runner.sh <bench_name> <lo> <hi> <step_fn> <percent_change_in_change_prop>"
    echo "- Size of input varies from <lo> to <hi>(inclusive). `i` tracks the current value of the loop. So "
    echo "- The <step_fn> parameter is supposed to be a string, which will be evaluated. For example: the default"
    echo "<step_fn> is \"i*10\", so whatever is passed should be something like that. "
    echo "- If \$RUNS is not specified, it'll take the default value(10)"
    exit 1
fi

RUNS=${RUNS:-10}
bench_name=$1
lo=$2
hi=$3
step_fn=${4:-"i*10"}
percent_changes=${5:-5}
i=$lo

while [ $i -le $hi ]; do
    changes=$((i*percent_changes/100))
    (set -x ; dune exec --release -- test/$bench_name -r $RUNS -n $i -c $changes)
    i=$(eval "echo $(($step_fn))")
done
