from markov_utilities import *
from matplotlib.ticker import MaxNLocator
np.seterr(divide='ignore', invalid='ignore')

# data
df = pd.read_csv("../data/markov.csv")
df = df[["group","s0","s1","s2","s3"]]
df = np.asarray(df)

# partial monitoring
g21 = get_states_matrix(df, 21)
g22 = get_states_matrix(df, 22)
g23 = get_states_matrix(df, 23)
g24 = get_states_matrix(df, 24)
matrices = [g21, g22, g23, g24]
m = get_net_transitions(matrices)
plot_net_transitions(m, "Partial Monitoring")

# full monitoring
g31 = get_states_matrix(df, 31)
g32 = get_states_matrix(df, 32)
g33 = get_states_matrix(df, 33)
g34 = get_states_matrix(df, 34)
matrices = [g31, g32, g33, g34]
m = get_net_transitions(matrices)
plot_net_transitions(m, "Full Monitoring")