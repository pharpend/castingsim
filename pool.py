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
        
        all_genetic = list(self.genetic_population) + list(other.genetic_population)
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

        iqs = [g+e for g, e in zip(self.genetic_population, self.env_population)]
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

        TODO: Have number of kids determined by Poisson distribution with mu=2.
        '''
        self.persons = [(g, e)
                        for g, e in zip(self.genetic_population,
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
        the genetic values of the two mates. It produces two children per pair.
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
        all_genetic_vals = []
        all_env_vals = []
        for pair_num in range(n_pairs):
            person1 = people_ls1[pair_num]
            person2 = people_ls2[pair_num]
#           print(person1)

            person1_genetic = person1[0]
            person2_genetic = person2[0]
            
            # make the genetic random seeds for the kid
            genetic_vals = [person1_genetic, person2_genetic]
            genetic_mean = np.mean(genetic_vals)
            genetic_std = np.std(self.genetic_population)*(2**0.5)
            
            # the genetic values for the two kids
            child1_genetic_val = r.gauss(genetic_mean, genetic_std)
            child2_genetic_val = r.gauss(genetic_mean, genetic_std)
#           print(child1_genetic_val)

            child1_env_val = r.gauss(env_mean, env_std)
            child2_env_val = r.gauss(env_mean, env_std)

            all_genetic_vals.append(child1_genetic_val)
            all_genetic_vals.append(child2_genetic_val)
            all_env_vals.append(child1_env_val)
            all_env_vals.append(child2_env_val)
            '''
            child1 = [child1_genetic_val, child1_env_val]
            child2 = [child2_genetic_val, child2_env_val]
            # print(child1)

            children.append(child1)
            children.append(child2)
            '''
        # end for

        all_genetic_vals = np.array(all_genetic_vals)
        all_env_vals = np.array(all_env_vals)
        
        return Pool(all_genetic_vals, all_env_vals)
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
            # Get the genetic values
            genetic = [vals[0] for vals in group]
            env = [vals[1] for vals in group]
            new_pool = Pool(genetic, env)
            groups_all.append(new_pool)

        return groups_all
    # end of partition()
        
# end of class
    
