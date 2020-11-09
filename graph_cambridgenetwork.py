# Create plot of the graph
# Necessary files for input;
# 1. adjacency_matrix.csv: adjacency matrix indicating which nodes are connected
# 2. colours: colour of the nodes
# 3. positions.txt: position of the nodes
# The order of nodes doesn't have to be the same in all the files

import matplotlib.pyplot as plt
import networkx as nx
import numpy as np
import pandas as pd

## Read in adjacency matrix
adj_mat = pd.read_csv("in_files/adjacency_matrix.csv", sep=",")
adj_mat = adj_mat.drop(axis=1, labels='Name')
num_nodes = len(adj_mat.columns)
labels_idx = {i: adj_mat.columns[i] for i in range(num_nodes)}
labels = {adj_mat.columns[i]: adj_mat.columns[i] for i in range(num_nodes)}
adj_mat_np = np.array(adj_mat)

## Create graph
G = (nx.convert_matrix.from_numpy_matrix(adj_mat_np))
G = nx.relabel_nodes(G, {i: labels_idx[i] for i in range(num_nodes)})

## Read in the positions and the colours of the nodes
pos = pd.read_csv("in_files/positions.txt", sep="\t", header=None)
colours = pd.read_csv("in_files/colours.txt", sep="\t", header=None)

## re-order
pos = pos.reindex([pos[0].tolist().index(j) for j in adj_mat.columns.tolist()])
colours = colours.reindex([colours[0].tolist().index(j) for j in adj_mat.columns.tolist()])
pos_dict = {pos.loc[i][0]: np.array([pos.loc[i][1], pos.loc[i][2]]) for i in range(num_nodes)}

## convert numbers to colours
colours_list = ([['blue', 'red', 'yellow', 'orange', 'green'][i] for i in colours[1]])

## Plot
nx.draw(G, pos_dict,  node_color=colours_list, node_size=400, alpha=0.2, cmap=plt.cm.Blues)
nx.draw_networkx_labels(G, pos_dict, labels, font_size=7, alpha=1)
plt.text(43.5,-9,s='Lena Morrill 2020',size=7, horizontalalignment='center')#, bbox=dict(facecolor='white', alpha=0.5))
plt.xlim(-35,50)
plt.ylim(-10,35)
plt.savefig("figures/Cambridge_initiatives_climate_and_sustainability.pdf")
plt.savefig("figures/Cambridge_initiatives_climate_and_sustainability.png", dpi=300)
