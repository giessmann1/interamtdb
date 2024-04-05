import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import statsmodels.api as sm
import numpy as np

# Load data
data = pd.DataFrame({
    'Sector_preference': [1, 1, 0, 0],
    'Signaling_value': [4, -4, 4, -4],
    'Intention_to_apply': [5, 0, 0, 5]
})

# Regression analysis (using seaborn)
sns.lmplot(x='Sector_preference', y='Intention_to_apply', hue='Signaling_value', data=data)
plt.show()