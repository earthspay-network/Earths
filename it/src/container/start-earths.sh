#!/bin/bash

trap 'kill -TERM $PID' TERM INT
echo Options: $EARTHS_OPTS
java $EARTHS_OPTS -jar /opt/earths/earths.jar /opt/earths/template.conf &
PID=$!
wait $PID
trap - TERM INT
wait $PID
EXIT_STATUS=$?
