import numpy as np
import quantecon as qe
import matplotlib.pyplot as plt
import pandas as pd

def get_states_matrix(array, group_n):
    """
    input: 
        - array: numpy array of raw data, all groups. first column has group number
        - group_n: group number
    output: numpy array for one group; shape is states x states
    """
    group_array = array[array[:,0] == group_n]
    group_array = group_array[:,1:] # drop the group column; array only of states
    dynamics = np.where(group_array==1)[1] # np.where() returns a tuple; first (0) is row index, second (1) is column
    n_states = group_array.shape[1]
    P = np.zeros((n_states,n_states))
    for i in range(0,len(dynamics)-1): 
        P[dynamics[i],dynamics[i+1]] = P[dynamics[i],dynamics[i+1]] + 1
    return(P)

def normalize(array):
    """
    input: list or array
    output: row-normalized numpy array
    """
    array = np.asarray(array)
    row_sums = array.sum(axis=1)
    normalized_array = array / row_sums[:, np.newaxis]
    normalized_array[np.isnan(normalized_array)] = 0
    normalized_array = normalized_array[normalized_array.sum(axis=1) > 0,:]
    normalized_array = normalized_array[:, normalized_array.sum(axis=0) > 0]
    return(normalized_array)


def get_stochastic_matrices(treatment):
    """
    input: treatment list of lists or arrays; each element is a group
    output: treatment list of stochastic numpy arrays
    """
    stochastic_matrices = []
    for matrix in treatment:
        x = normalize(matrix)
        stochastic_matrices.append(x)
    return(stochastic_matrices)

def simulate_mc(treatment, n_iter=None, reps=None):
    """
    input: treatment list of stochastic numpy arrays
    output: list of simulated chains for each group
    bugs:
        - reps > 1 breaks the list of states
    """
    N = n_iter
    simulations = []
    for matrix in treatment:
        # run simulations
        mc = qe.MarkovChain(matrix)
        X = mc.simulate(ts_length=N, num_reps=reps)
        # get fraction of time in each state
        n_states = matrix.shape[0]
        states = range(n_states)
        s = []
        for state in states:
            x = (X == state).cumsum() / (1 + np.arange(N, dtype=float))
            s.append(x)
        simulations.append(s)
    return(simulations)

def get_stationary_states(treatment):
    """
    input: treatment list of stochastic numpy arrays
    output: list of numpy arrays; each array is a group's stationary distribution
    """
    stationary_distributions = []
    for matrix in treatment:
        mc = qe.MarkovChain(matrix)
        s = mc.stationary_distributions
        stationary_distributions.append(s)
    return(stationary_distributions)


def plot_simulation(simulation_results, treatment, missing_state=None, exception=None):
    """
    input: 
        - simulation_results
            - list of simulation results; each element is a list of simulation results for one group
        - treatment
            - treatment name; a string for the plot title
        - missing_state
            - 
    output: 2x2 plot; each plot is a group
    """
    n_states = 4
    fig, axes = plt.subplots(nrows=2, ncols=2, figsize=(12, 8))
    fig.subplots_adjust(hspace=.5)
    fig.suptitle(treatment, fontsize=20)
    groups = ["Group 1", "Group 2", "Group 3", "Group 4"]
    for ax, group_name, state in zip(axes.flat, groups, simulation_results):
        if len(state) == n_states:
            ax.plot(state[0], color="orange", label="$S_1$", linewidth=2)
            ax.plot(state[1], color="red", label="$S_2$", linewidth=2)
            ax.plot(state[2], color="green", label="$S_3$", linewidth=2)
            ax.plot(state[3], color="blue", label="$S_4$", linewidth=2)
            ax.legend(loc='upper right')
            ax.set_title(group_name, fontsize=14)
            ax.set_xlabel("Simulation runs", fontsize=14)
            ax.set_ylabel("$P(i,j)$", fontsize = 14)
        else:
            if missing_state == 0 and treatment == "Partial Monitoring" and group_name != "Group 4":
                ax.plot(state[0], color="red", label="$S_2$", linewidth=2)
                ax.plot(state[1], color="green", label="$S_3$", linewidth=2)
                ax.plot(state[2], color="blue", label="$S_4$", linewidth=2)
                ax.legend(loc='upper right')
                ax.set_title(group_name, fontsize=14)
                ax.set_xlabel("Simulation runs", fontsize=14)
                ax.set_ylabel("$P(i,j)$", fontsize = 14)   
            elif treatment == "Partial Monitoring" and group_name == "Group 4":
                ax.plot(state[0], color="orange", label="$S_1$", linewidth=2)
                ax.plot(state[1], color="green", label="$S_3$", linewidth=2)
                ax.plot(state[2], color="blue", label="$S_4$", linewidth=2)
                ax.legend(loc='upper right')
                ax.set_title(group_name, fontsize=14)
                ax.set_xlabel("Simulation runs", fontsize=14)
                ax.set_ylabel("$P(i,j)$", fontsize = 14)
            elif missing_state == 0 and treatment != "Partial Monitoring" and group_name != "Group 4":
                ax.plot(state[0], color="red", label="$S_2$", linewidth=2)
                ax.plot(state[1], color="green", label="$S_3$", linewidth=2)
                ax.plot(state[2], color="blue", label="$S_4$", linewidth=2)
                ax.legend(loc='upper right')
                ax.set_title(group_name, fontsize=14)
                ax.set_xlabel("Simulation runs", fontsize=14)
                ax.set_ylabel("$P(i,j)$", fontsize = 14)
        fig.show()
        
def get_net_transitions(treatment):
    """
    input: list of raw matrices; each element is a group
    output: list of net transition matrices; each element is a group
    """
    stochastic_matrices = get_stochastic_matrices(treatment)
    net_transitions = []
    for matrix in stochastic_matrices:
        net_transition = matrix - matrix.T
        net_transitions.append(net_transition)
    return(net_transitions)         

def plot_net_transitions(treatment, title):
    """
    input: 
        treatment: list of net transition matrices; each element is a group
        title: string, the title of the plot
    output: 2x2 contour plot; each plot visualizes a group's net transition matrix
    """
    groups = ["Group 1", "Group 2", "Group 3", "Group 4"]
    ticks = ["x1", "x2", "x3", "x4"]
    fig, axes = plt.subplots(nrows=2, ncols=2, figsize=(12, 8))
    fig.subplots_adjust(hspace=.5)
    fig.suptitle(title, fontsize=20)
    for ax, group, matrix in zip(axes.flat, groups, treatment):
        im = ax.contourf(matrix, vmin=-1, vmax=1)
        ax.xaxis.set_major_locator(MaxNLocator(integer=True))
        ax.yaxis.set_major_locator(MaxNLocator(integer=True))
        ax.set_xticklabels(ticks)
        ax.set_yticklabels(ticks)
        ax.set_title(group, fontsize=14)
        ax.set_xlabel("$P(i,j)$", fontsize=14)
        ax.set_ylabel("$P(j,i)$", fontsize=14)
        plt.colorbar(im, ax=ax) 
    plt.show()