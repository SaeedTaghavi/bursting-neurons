import numpy as np
import matplotlib.pyplot as plt

with open('output.txt') as datafile:
    lines = (line for line in datafile if not line.startswith('#'))
    data = np.loadtxt(lines)

time=data[:,1]
p=data[:,2]
Iext=data[:,5]
plt.plot(time,p,label="p")
plt.plot(time,Iext,label="Iext")
plt.xlabel("time")
plt.ylabel("p,Iext")
plt.legend()
plt.savefig("p-t.png")
# plt.show()
exit(0)

