#!/usr/bin/env python3

from sys import argv
from subprocess import check_output, CalledProcessError
from os import system

BRIGHTNESS_CMD: str = "mac-brightnessctl"

Number = int | float


def clamp[T: Number](n: Number, lower: Number, upper: Number) -> Number:
    """Clamp a number between a lower and upper bound"""
    return max(min(n, upper), lower)


def check_if_brightness_cmd_exists() -> None:
    try:
        check_output(f"{BRIGHTNESS_CMD}", shell=True, text=True)
    except CalledProcessError:
        exit(1)


def main() -> None:
    SCRIPT_NAME: str = argv[0]

    if len(argv) != 2:
        print(f"Usage: {SCRIPT_NAME} +/-:<number>")
        exit(1)
    else:
        arg: str = argv[1]

        modifier: str = arg[0]

        if modifier != "+" and modifier != "-":
            print(f"Usage: {SCRIPT_NAME} +/-:<number>")
            exit(1)
        else:
            try:
                amount: float = float(arg[1:])

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
                print(f"Usage: {SCRIPT_NAME} +/-:<number>")
                exit(1)


if __name__ == "__main__":
    check_if_brightness_cmd_exists()
    main()
