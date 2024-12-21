import matplotlib.pyplot as plt
import numpy as np
from matplotlib import font_manager

font_path = '/Users/matteoramina/Library/Fonts/lmroman12-regular.otf'
font_prop = font_manager.FontProperties(fname=font_path)
plt.rcParams['font.family'] = font_prop.get_name()

# Data for the lines
time_points = [-1.6, -1, 0, 1, 1.6]  # T_{-1}, T_0, T_1
solid_navy_line = [np.nan, 9, 12, 15, np.nan]
solid_purple_line = [np.nan, 3, 6, 12, np.nan]
dashed_purple_line = [np.nan, 3, 6, 9, np.nan]

# Create figure and axis
fig, ax = plt.subplots(figsize=(8, 6))

# Add lines
ax.plot(time_points, solid_navy_line, color='navy', linewidth=2)
ax.plot(time_points, solid_purple_line, color='purple', linewidth=2)
ax.plot(time_points, dashed_purple_line, color='purple', linestyle='--', linewidth=2)
for t in [-1, 0, 1]:
    ax.axvline(x=t, color='gray', linestyle=':', alpha=0.6)

# Add annotation
ax.annotate(
    r'$\mathregular{\tau_{DID}}$', 
    xy=(1.1, 10.5), 
    xytext=(1.4, 10.5),
    xycoords='data', 
    fontsize=12,
    ha='center', 
    va='center',
    bbox=None,
    arrowprops=dict(
        arrowstyle='-[, widthB=3.2, lengthB=1', 
        lw=1.5,
        color='k'
    ),
    fontproperties=font_prop
)

# Format labels
ax.set_xlabel('\nTime', fontsize=14, fontproperties=font_prop)
ax.set_ylabel('Outcome\n', fontsize=14, fontproperties=font_prop)
ax.set_xticks(time_points)
ax.set_xticklabels(['', r'$\mathregular{T_{-1}}$', r'$\mathregular{T_{0}}$', 
                    r'$\mathregular{T_{1}}$', ''], fontsize=12, 
                   fontproperties=font_prop)
ax.set_yticklabels([], fontproperties=font_prop)

# Remove elements
ax.grid(False)
ax.legend().set_visible(False)
ax.set_title('')
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)

plt.savefig('/Users/matteoramina/Documents/education/hasselt/isds/paper/isd_essay_unmeasured_confounders/shared_outputs/did_graph.png', dpi=300)

# Show the plot
plt.show()