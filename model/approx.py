import numpy as np

import matplotlib.pyplot as plt
import seaborn as sns

from fixedpoint import FixedPoint as fp

import math

sns.set_style('darkgrid')

def str2fl (din, m, n):
    dout = []
    for din_p in din:
        dout.append(float(fp('0x'+din_p[4:], m=m, n=n, signed=1, str_base=16)))

    return dout

def fl2str (din, m, n):
    dout = []

    for din_p in din:
        dout.append(str(m+n)+"'h"+str(fp(din_p, m=m, n=n, str_base=16)))

    return dout

def computeK (din):
    dout = []

    for i in range(len(din)-1):
        dout.append(1/(din[i]-din[i+1]))

    return dout
