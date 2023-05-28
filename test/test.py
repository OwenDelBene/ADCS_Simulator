import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_csv('test.csv')
x = df.iloc[:, 0].to_numpy()
y = df.iloc[:, 1].to_numpy()
z = df.iloc[:, 2].to_numpy() 

t = np.linspace(0,10, x.size)
plt.plot(t, x)
plt.plot(t, y)
plt.plot(t, z)
plt.show()