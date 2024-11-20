import numpy as np
import matplotlib.pyplot as plt

# Total quantities
total_books = 80
total_wine = 40

# Create the Edgeworth box
fig, ax = plt.subplots(figsize=(8, 6))

# Set limits
ax.set_xlim(0, total_books)
ax.set_ylim(0, total_wine)

# Draw the box
ax.plot([0, total_books], [0, 0], 'k')
ax.plot([0, total_books], [total_wine, total_wine], 'k')
ax.plot([0, 0], [0, total_wine], 'k')
ax.plot([total_books, total_books], [0, total_wine], 'k')

# Initial endowment point E
E_books = 60  # Morris's books
E_wine = 10   # Morris's wine
ax.plot(E_books, E_wine, 'ko', label='Initial Endowment E')

# Morris's indifference curve: b + w = 70
b_morris = np.linspace(0, 70, 100)
w_morris = 70 - b_morris
ax.plot(b_morris, w_morris, 'r-', label="Morris's Indifference Curve")

# Philip's indifference curve: (80 - b) * (40 - w) = 600
# Since Philip's quantities are measured from the upper right
# We need to transform the variables
U_philip = 600
b_vals = np.linspace(E_books, total_books, 100)
w_philip = total_wine - (U_philip / (total_books - b_vals))
ax.plot(b_vals, w_philip, 'b-', label="Philip's Indifference Curve")

# Annotate the initial endowment
ax.annotate('E', xy=(E_books, E_wine), xytext=(E_books + 2, E_wine + 2))

# Labels and title
ax.set_xlabel('Morris Books')
ax.set_ylabel('Wine (Morris from bottom, Philip from top)')
ax.set_title('Edgeworth Box for Morris and Philip')

# Customize ticks to show Philip's perspective
ax_secondary_x = ax.twiny()
ax_secondary_x.set_xlim(ax.get_xlim())
ax_secondary_x.set_xticks(np.linspace(0, total_books, 5))
ax_secondary_x.set_xticklabels([str(total_books - x) for x in ax.get_xticks().tolist()])
ax_secondary_x.set_xlabel("Philip's Books")

ax_secondary_y = ax.twinx()
ax_secondary_y.set_ylim(ax.get_ylim())
ax_secondary_y.set_yticks(np.linspace(0, total_wine, 5))
ax_secondary_y.set_yticklabels([str(total_wine - y) for y in ax.get_yticks().tolist()])
ax_secondary_y.set_ylabel("Philip's Wine")

# Add legend
ax.legend(loc='upper right')

# Show grid
ax.grid(True, linestyle='--', alpha=0.5)

plt.show()
