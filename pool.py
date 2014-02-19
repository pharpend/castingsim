#!/usr/bin/env python3

'''
Pool - this is code for a group of people
Any copyright is dedicated to the Public Domain.
'''

import random as r
import numpy as np


class Pool:

    '''
    This is a class for a group of people for the genetic casting simulator.
    '''

    def __add__(self, other):
        '''
        Add two Pools together. Overloads the + operator
        '''

        all_genetic = list(self.genetic_population) + list(
            other.genetic_population)
        all_env = list(self.env_population) + list(other.env_population)

        all_genetic = np.array(all_genetic)
        all_env = np.array(all_env)

        return Pool(all_genetic, all_env)
    # end of + overloading

    def __init__(self,
                 genetic_population: np.array,
                 env_population: np.array):
        '''
        This sets instance variables equal to arguments, then calls make().
        '''

        self.genetic_population = genetic_population
        self.env_population = env_population

        self.make()

    # end of constructor

    def drain(self):
        '''
        Empties the pool.
        '''

        self.genetic_population = np.array([])
        self.env_population = np.array([])
    # end of drain()

    def get_distribution(self):
        '''
        This simply returns a list contiaining the IQ of each person.
        '''

        iqs = [g + e for g, e in zip(
            self.genetic_population, self.env_population)]
        return iqs
    # end of get_distribution()

    def make(self):
        '''
        This makes the Pool.
        '''

        '''
        This list comprehension is admittedly a bit dense. Basically, it makes a
        tuple associating each genetic value with an environmental value. Then, it
        makes a list of those tuples, and convert it into a numpy array.
        '''
        self.persons = [(g, e)
                        for g, e in zip(self.genetic_population,
                                        self.env_population)]
        self.persons.sort()
        self.persons = np.array(self.persons)
    # end of make()

    def poisson_mate(self):
        '''
        Mate the pool randomly.
        '''

        '''
        So, the way this works is, the algorithm picks two random people out of
        the pool, mates them, produces one child. It does this until there are
        enough children.

        This is roughly based on the former mate_pool() function. Look at
        previous versions of this file to get it.
        '''

        # Environmental normal values
        pool_env_mean = np.mean(self.env_population)
        pool_env_std = np.std(self.env_population)

        # Values for the children
        children_genetic_vals = []
        children_env_vals = []

        # Do this until we have enough children
        while len(children_genetic_vals) < len(self.persons):
            # Choose two people
            parents = [r.choice(self.persons) for i in range(2)]

            # Get the genetic normal values
            parent_genetic_vals = np.array([parent[0] for parent in parents])
            parent_genetic_mean = np.mean(parent_genetic_vals)
            population_genetic_std = np.std(self.genetic_population) * (np.sqrt(2)/2)

            # A note. The square root of 2 was a fudge factor I was told to
            # implement. I won't justify it.

            # Make the children
            child_genetic_val = np.random.normal(parent_genetic_mean,
                                                 population_genetic_std)
            child_env_val = np.random.normal(pool_env_mean, pool_env_std)

            # Add them
            children_genetic_vals.append(child_genetic_val)
            children_env_vals.append(child_env_val)
        # end of loop

        # Make a pool, return it
        return Pool(children_genetic_vals, children_env_vals)
    # end of poisson_mate

    def partition(self,
                  group_sizes: list):
        '''
        Partitions the pool into groups of a specified size.
        '''

        '''
        This code may be a bit difficult to read, but here goes. It partitions
        the pool into a set number of groups. Then, it constructs Pool objects
        out of those groups, and returns a list of those.
        '''

        # Alright, so, this partitions the pool
        groups_array = []
        j = 0
        for size in group_sizes:
            group = []
            for i in range(int(size)):
                group.append(self.persons[i + j])
            groups_array.append(group)
            j += int(size)

        groups_all = []
        # For each group
        for group in groups_array:
            # Get the genetic values
            genetic = [vals[0] for vals in group]
            env = [vals[1] for vals in group]
            new_pool = Pool(genetic, env)
            groups_all.append(new_pool)

        return groups_all
    # end of partition()

# end of class
