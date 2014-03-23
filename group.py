#!/usr/bin/python3

"""Group - class for a group

Group of agents

"""

import numpy as np


class Group:
    def __init__(self, n, traits):
        self.n = n
        self.traits = traits

    def __iter__(self):
        for a in self.agents:
            yield a

    def means(self):
        return {tn: np.mean(np.array([agent.traits[tn]
                                      for agent in self.agents]))
                for tn in self.traits}

    def stds(self):
        return {tn: np.std(np.array([agent.traits[tn]
                                    for agent in self.agents]))
                for tn in self.traits}
