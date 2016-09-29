#!/usr/bin/python

import numpy as np
import matplotlib.pyplot as plt

filename = "info/offenseFitness.dat"

with open(filename) as f:
    data = f.read()

data = data.split("\n")

# drop empty row
data = data[1:-1]

num_lines = sum (1 for line in data)

x  = range(0,num_lines)
y1 = [row.split(" ")[0] for row in data]
y2 = [row.split(" ")[1] for row in data]

fig = plt.figure(figsize=(18,12))

ax1 = fig.add_subplot(111)

ax1.set_title("Best-Mean-Fitness of neural evolution with CO:25%, MU:10%, phi:3, trails: 25, only goal fitness")
ax1.set_xlabel("Generations")
ax1.set_ylabel("Fitness")

ax1.plot(x,y1, label="best individual")
ax1.plot(x,y2, label="mean of all individuals")

ax1.legend(loc='upper left')

fig.savefig("current/best-mean-fitness-offense.png")