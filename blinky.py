#!/usr/bin/env python3

from sys import argv
from subprocess import check_output, CalledProcessError
from os import system

# Assumes that the "mac-brightnessctl" is installed via homebrew
BRIGHTNESS_CMD = "/opt/homebrew/bin/mac-brightnessctl"


def clamp(n, lower, upper):
    """Clamp a number between a lower and upper bound"""
    return max(min(n, upper), lower)


def check_if_brightness_cmd_exists():
    try:
        check_output(f"{BRIGHTNESS_CMD}", shell=True, text=True)
    except CalledProcessError:
        exit(1)


def main() -> None:
    SCRIPT_NAME = argv[0]

    USAGE_WARNING = f"{SCRIPT_NAME}: usage: -/+<amount>"

    if len(argv) != 2:
        print(USAGE_WARNING)
        exit(1)
    else:
        arg = argv[1]

        modifier = arg[0]

        if modifier != "-" and modifier != "+":
            print(USAGE_WARNING)
            exit(1)
        else:
            try:
                amount = float(arg[1:])

                brightness = check_output(f"{BRIGHTNESS_CMD}", shell=True, text=True)

                current_brightness = float(brightness.split(": ")[1])

                current_brightness = (
                    current_brightness + amount
                    if modifier == "+"
                    else current_brightness - amount
                )

                current_brightness = clamp(current_brightness, 0.0, 1.0)

                system(f"{BRIGHTNESS_CMD} {current_brightness}")

                exit(0)
            except ValueError:
                print(USAGE_WARNING)
                exit(1)


if __name__ == "__main__":
    check_if_brightness_cmd_exists()
    main()
