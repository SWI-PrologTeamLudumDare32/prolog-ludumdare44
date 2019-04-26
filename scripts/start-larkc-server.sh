#!/bin/bash

. envvars.sh

# ../cyc-jrtl-with-commonlisp-20190124/platform/from_swipl.pl

cd $LD44ROOT/$LARKC_CL/platform && swipl -f from_swipl.pl
