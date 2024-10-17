library(BoolNet)
network <- generateRandomNKNetwork(6, 3, "homogeneous" )
print(network)
network_interactions <- network$interactions
print(network_interactions)