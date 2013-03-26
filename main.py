#!/usr/bin/env python3

'''
Casting Simulator - This program simulates genetic casting in a meritocracy
Copyright (C) 2013 Peter Harpending
'''

'''
This program is a discrete process.

1. Put everyone in one pool (n people).
2. Put people into (g) discrete groups of size (n/g).
3. Have everyone in each group mate and die such that the population remains at
(n/g).
4. Run whatever intermittent data collection is wanted.
5. Repeat this process T times.
'''

import sys
import traceback
import numpy as np
import pool

# First, here are some global_variables
global number_of_people
global number_of_groups
global number_of_runs
global mean
global standard_deviation
global heritability
global genic_population
global env_population
global general_pool
global individual_groups

def intermittent_process():
	'''
	This is the code that runs between the groups getting split up, and then put
	back together. The idea is, people can put whatever stat-collecting code in
	here.

	Currently, it just has each group mate.
	'''

	global individual_groups

	# Each group should mate
	[group.mate_pool() for group in individual_groups]
# end of intermittent_process()
	
def main():
	'''
	This parses command line arguments, then runs sim().
	'''
	
	global number_of_people
	global number_of_groups
	global number_of_runs
	global mean
	global standard_deviation
	global heritability

	try:
		number_of_people = int(sys.argv[1])
		number_of_groups = int(sys.argv[2])
		number_of_runs = int(sys.argv[3])
		mean = float(sys.argv[4])
		standard_deviation = float((sys.argv[5]))
		heritability = float(sys.argv[6])
		
		sim()
		
	# If the arguments are invalid or don't exist
	except Exception as e:
		print(traceback.format_exc())
		
		help_message = 'Usage: python main.py n g r m s h\n'
		help_message += 'n = sample size\n'
		help_message += 'g = number of groups\n'
		help_message += 'r = number of runs\n'
		help_message += 'm = mean\n'
		help_message += 's = standard deviation\n'
		help_message += 'h = heritability\n'
		
		print(help_message)
		exit()
# end of main()

def make_pool():
	'''
	This function makes, and returns a list of Person objects.
	'''

	global number_of_people
	global number_of_groups
	global number_of_runs
	global mean
	global standard_deviation
	global heritability
	global genic_population
	global env_population

	# Compute the genic mean; easy.
	genic_mean = mean*heritability
	
	# The genic std is a bit harder
	variance = standard_deviation**2
	genic_variance = heritability*variance
	genic_standard_deviation = genic_variance**0.5

	# The environmential variables are whatever is left
	env_mean = mean - genic_mean
	env_variance = variance - genic_variance
	env_standard_deviation = env_variance**0.5

	# First, make the population according to a normal distribution
	genic_population = np.random.normal(genic_mean,
										genic_standard_deviation,
										number_of_people)
	env_population = np.random.normal(env_mean,
									  env_standard_deviation,
									  number_of_people)

	return pool.Pool(genic_population, env_population)
# end of make_pool()
	

def sim():
	'''
	This function actually runs the simulation.

	So, assuming the heritability is h, and the trait we are measuring is IQ,
	the genic mean for IQ is h*(mean_iq).

	The variance for IQ would be standard_deviation**2. So, the genic standard
	deviation is sqrt(standard_deviation**2)*.8)
	'''
	
	# Global variables
	global number_of_people
	global number_of_groups
	global number_of_runs
	global mean
	global standard_deviation
	global heritability
	global general_pool
	global individual_groups

	# Make the initial people objects
	general_pool = make_pool()
	
	# Run the simulation
	for i in range(number_of_runs):
		# Split up the groups
		individual_groups = general_pool.partition(number_of_groups)
		
		# Run whatever is in intermittent_process()
		intermittent_process()
		
		general_pool.drain()
		# Conglomerate the pools
		for group in individual_groups:
			general_pool += group
			
	# end of for
		
# end of make()
	
	

if __name__ == '__main__':
	main()
