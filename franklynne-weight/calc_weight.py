#!/usr/bin/env python3


"""
@file

Fit logarithmic function to samples of Frank's weight over time

This script should be general enough to be rerun without modification when new data is
collected, and provide a better estimate.



@author  Hamish Morgan
@date    17/10/2021
@license BSD
"""


import csv
import datetime
import math
import sys

import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import numpy as np
import scipy.optimize


def timestamp_from_date(date: str) -> float:
    """
    Convert date string to seconds since epoch

    @param[in] date Date as string, e.g. "2021-05-27"
    @return Date as timestamp (seconds since epoch)
    """
    return datetime.datetime.fromisoformat(date).timestamp()


def read_data(filename: str = "weights.csv") -> tuple[np.ndarray, np.ndarray]:
    """
    Read CSV data from @a filename, convert to numpy arrays for x and y
    """
    with open(filename) as weights_file:
        data = list(csv.reader(weights_file))
    x = np.array([timestamp_from_date(row[0]) for row in data])
    y = np.array([              float(row[1]) for row in data])
    return x, y


def weight(x: float, A: float, B: float, C: float) -> float:
    """
    Compute weight function at time @a x
    """
    return A * np.log(x - B) + C


def fit_to_data(x: np.ndarray, y: np.ndarray) -> np.ndarray:
    """
    Fit @a func to data in @a x and @a y

    Create an initial estimate for parameters, because timestamps are very big
    """
    p0 = np.array([1.0, x[0] - 100, 0.0])
    popt, _ = scipy.optimize.curve_fit(f=weight, xdata=x, ydata=y, p0=p0)
    return popt


def as_date(timestamp: float) -> datetime.date:
    """
    Convert a timestamp as seconds since epoch to date object for plotting
    """
    return datetime.datetime.fromtimestamp(timestamp).date()


def plot_results(popt: np.ndarray, x: np.ndarray, y: np.ndarray) -> None:
    """
    Display raw data and fitted curve together
    """
    seconds_per_year = 60 * 60 * 24 * 365
    lowest_x = x[0] - seconds_per_year * 1/12  # weight is undefined for x <= popt[0]
    highest_x = x[-1] + seconds_per_year * 1
    xtest = np.linspace(lowest_x, highest_x, 100)
    ytest = weight(xtest, *popt)
    plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%d/%m/%Y'))
    plt.gca().xaxis.set_major_locator(mdates.DayLocator(interval=50))
    plt.plot(list(map(as_date, x)),     y,     "ko", label="raw data")
    plt.plot(list(map(as_date, xtest)), ytest, "r-", label="fitted function")
    plt.gcf().autofmt_xdate()
    plt.grid(True, which="both")
    plt.xlabel("Date")
    plt.ylabel("Weight (kg)")
    plt.show()


def main() -> int:
    x, y = read_data("weights.csv")
    optimal_params = fit_to_data(x, y)
    plot_results(optimal_params, x, y)
    return 0


if __name__ == "__main__":
    sys.exit(main())
