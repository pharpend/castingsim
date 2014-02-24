#!/usr/bin/python3

"""Group - class for a group

Group of agents

"""

from agent import Agent


class Group:
    def __init__(self, n, traits):
        self.n = n
        self.traits = traits
        self.agents = (Agent(**{tn: tv.get_variate()
                                for tn, tv in self.traits.items()})
                       for i in range(n))

    def __iter__(self):
        return self.agents
