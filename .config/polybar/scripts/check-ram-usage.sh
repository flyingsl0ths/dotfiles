#!/usr/bin/env bash

free -h --si | grep Mem | awk '{print ($3)}'
