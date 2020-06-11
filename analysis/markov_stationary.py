from markov_utilities import *
np.seterr(divide='ignore', invalid='ignore')

# data
df = pd.read_csv("../data/markov.csv")
df = df[["group","s0","s1","s2","s3"]]
df = np.asarray(df)

#=========================
# partial monitoring
#=========================
# set up
g21 = get_states_matrix(df, 21)
g22 = get_states_matrix(df, 22)
g23 = get_states_matrix(df, 23)
g24 = get_states_matrix(df, 24)
matrices = [g21, g22, g23, g24]

# check for empty states
for matrix in matrices:
    which_s = np.where(matrix.sum(axis=0)==0)[0]
    print("s = ",which_s)

# simulate 
stochastic_matrices = get_stochastic_matrices(matrices)    
simulations = simulate_mc(stochastic_matrices, n_iter=500, reps=1)
plot_simulation(simulations, "Partial Monitoring", missing_state=0)
get_stationary_states(stochastic_matrices)    

#=========================
# full monitoring
#=========================
# set up
g31 = get_states_matrix(df, 31)
g32 = get_states_matrix(df, 32)
g33 = get_states_matrix(df, 33)
g34 = get_states_matrix(df, 34)
matrices = [g31, g32, g33, g34]

# check for empty states
for matrix in matrices:
    which_s = np.where(matrix.sum(axis=0)==0)[0]
    print("s = ",which_s)
stochastic_matrices = get_stochastic_matrices(matrices)    

# simulate 
simulations = simulate_mc(stochastic_matrices, n_iter=500, reps=1)
plot_simulation(simulations, "Full Monitoring", missing_state=0)
get_stationary_states(stochastic_matrices)    