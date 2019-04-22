#!/bin/bash

width=10
height=10
if [ "$1" != "" ]; then
	width=$1
fi

if [ "$2" != "" ]; then
	height=$2
fi

java -jar ../java/Ld41.jar $width $height
