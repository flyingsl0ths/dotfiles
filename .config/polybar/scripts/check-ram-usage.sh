#!/usr/bin/bash

free -m | grep Mem | awk '{print ($3)}'
