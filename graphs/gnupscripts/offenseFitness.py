#!/usr/bin/python

import numpy as np
import matplotlib.pyplot as plt

filename = "info/offenseFitness.dat"

with open(filename) as f:
    data = f.read()

data = data.split("\n")

# drop empty row and add zero start step
data = ["0.0 0.0"] + data[0:-1]

num_lines = sum (1 for line in data)

x  = range(0,num_lines)
y1 = [float(row.split(" ")[0])/100.0 for row in data]
y2 = [float(row.split(" ")[1])/100.0 for row in data]

fig = plt.figure(figsize=(8,5))

ax1 = fig.add_subplot(111)

ax1.set_title("Best and mean winrate of the CoSyNE algorithm")
ax1.set_xlabel("Generations")
ax1.set_ylabel("Winrate in %")
ax1.set_ylim([0,50])
ax1.set_xlim([0,300])

plty1, = ax1.plot(x,y1, color="#ca7221") #, label="best individual")
plty2, = ax1.plot(x,y2, color="#003869") #, label="mean of all individuals")

ax1.legend((plty1, plty2), ("best individual", "mean of individuals"), loc="upper left", fontsize=10) # , bbox_to_anchor=(0.2, 1))

fig.savefig("current/normalized-fitness.png")