#!/usr/bin/python3

"""Generator - class for a generator

Takes a pool and a number of generations

"""


class Generator:
    def __init__(self, pool, ngens):
        self.pools = [pool, ]
        self.number_of_generations = ngens

    def __iter__(self):
        for p in self.pools:
            yield p

    def run(self):
        # n-1, because the first generation is already taken care of
        for i in range(self.number_of_generations - 1):
            self.pools.append(self.pools[i].mate())
