#!/usr/bin/env python3

'''
Pool - this is code for a group of people
Copyright (C) 2013 Peter Harpending
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
        
        all_genic = list(self.genic_population) + list(other.genic_population)
        all_env = list(self.env_population) + list(other.env_population)
        
        all_genic = np.array(all_genic)
        all_env = np.array(all_env)

        return Pool(all_genic, all_env)
    # end of + overloading

    def __init__(self,
                 genic_population: np.array,
                 env_population: np.array):
        '''
        This sets instance variables equal to arguments, then calls make().
        '''

        self.genic_population = genic_population
        self.env_population = env_population

        self.make()

    # end of constructor

    def drain(self):
        '''
        Empties the pool.
        '''

        self.genic_population = np.array([])
        self.env_population = np.array([])
    # end of drain()

    def get_distribution(self):
        '''
        This simply returns a list contiaining the IQ of each person.
        '''

        iqs = [g+e for g, e in zip(self.genic_population, self.env_population)]
        return iqs
    # end of get_distribution()

    def make(self):
        '''
        This makes the Pool.
        '''
        
        '''
        This list comprehension is admittedly a bit dense. Basically, it makes a
        tuple associating each genic value with an environmental value. Then, it
        makes a list of those tuples, and convert it into a numpy array.
        '''
        self.persons = [(g, e)
                        for g, e in zip(self.genic_population,
                                        self.env_population)]
        self.persons.sort()
        self.persons = np.array(self.persons)
        
    # end of make()

    def mate_pool(self):
        '''
        This has everyone in the Pool to mate. It returns another Pool.
        '''

        '''
        Here is the process: first, it shuffles the pool. Then, it splits the
        pool into two equal parts, each part being one sex. Then, it averages
        the genic values of the two mates. It produces two children per pair.
        '''

        # Environmental normal values
        env_mean = np.mean(self.env_population)
        env_std = np.std(self.env_population)
        
        # Shuffle the pool
        people_ls = list(self.persons)
        r.shuffle(people_ls)
        people_ls = np.array(people_ls)
#       print(people_ls)

        # Split it up
        people_ls1, people_ls2 = np.split(people_ls, 2)
        n_pairs = len(people_ls1)

        # Mate 
#       children = []
        all_genic_vals = []
        all_env_vals = []
        for pair_num in range(n_pairs):
            person1 = people_ls1[pair_num]
            person2 = people_ls2[pair_num]
#           print(person1)

            person1_genic = person1[0]
            person2_genic = person2[0]
            
            genic_vals = [person1_genic, person2_genic]
            genic_mean = np.mean(genic_vals)
            genic_std = np.std(genic_vals)
            
            child1_genic_val = r.gauss(genic_mean, genic_std)
            child2_genic_val = r.gauss(genic_mean, genic_std)
#           print(child1_genic_val)

            child1_env_val = r.gauss(env_mean, env_std)
            child2_env_val = r.gauss(env_mean, env_std)

            all_genic_vals.append(child1_genic_val)
            all_genic_vals.append(child2_genic_val)
            all_env_vals.append(child1_env_val)
            all_env_vals.append(child2_env_val)
            '''
            child1 = [child1_genic_val, child1_env_val]
            child2 = [child2_genic_val, child2_env_val]
#           print(child1)

            children.append(child1)
            children.append(child2)
            '''
        # end for

        all_genic_vals = np.array(all_genic_vals)
        all_env_vals = np.array(all_env_vals)
        
        return Pool(all_genic_vals, all_env_vals)
    # end of mate()
        
    def partition(self,
                  number_of_groups: int = 2):
        '''
        Partitions the pool into equal sized groups.
        '''
        
        '''
        This code may be a bit difficult to read, but here goes. It partitions
        the pool into a set number of groups. Then, it constructs Pool objects
        out of those groups, and returns a list of those.
        '''
        
        # Alright, so, this partitions the pool
        groups_array = np.split(self.persons, number_of_groups)
#       print(groups_array)

        groups_all = []
        # For each group
        for group in groups_array:
            # Get the genic values
            genic = [vals[0] for vals in group]
            env = [vals[1] for vals in group]
            new_pool = Pool(genic, env)
            groups_all.append(new_pool)

        return groups_all
    # end of partition()
        
# end of class
    
