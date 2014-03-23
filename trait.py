#!/usr/bin/python3

"""Trait - class for a trait

Takes a function that gets a new value for the trait

"""

import numpy as np


# Takes the genetic values of the parents, and the environmental values
def poisson_mate_function(parents, env_mean, env_var):
    parents_mean = np.mean(parents)
    parents_var = np.var(parents)
    parents_std = parents_var ** (1/2)
    parents_std_fudged = parents_std / np.sqrt(2)

    # A note. The square root of 2 was a fudge factor I was told to
    # implement. I won't justify it.

    child_genetic_val = np.random.normal(parents_mean, parents_std_fudged)
    child_env_val = np.random.normal(env_mean, env_var)

    return (child_genetic_val, child_env_val)


def normal_trait_variate(h):
    # h = heritability
    gen = np.random.normal(0, np.sqrt(h))
    env = np.random.normal(0, np.sqrt(1-h))
    return (gen, env)


class Trait:
    def __init__(self, name="Generic Normal Trait",
                 variate_function=normal_trait_variate,
                 heritability=0.8,
                 mate_function=poisson_mate_function):
        self.get_variate = lambda: variate_function(heritability)
        self.heritability = heritability
        self.__mate_function = mate_function
        self.name = name

    def get_distribution(self, n):
        return [self.get_variate(self.heritability) for i in range(n)]

    def get_env_pop(self, n):
        return [d[1] for d in self.get_distribution(n)]
