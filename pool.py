#!/usr/bin/python3

"""Pool - class for a pool

Takes a number of agents and a list of traits. Generates agents.

"""

from agent import Agent


class Pool:
    def __init__(self, group_sizes, **traits):
        self.group_sizes = group_sizes
        self.traits = traits
        print(traits)

    def get_groups(self):
        # This is pretty dense.  This generator loops through the list
        # of sizes. It then creates a number of agents
        # return ((Agent((t.get_variate() for t in self.traits))
        #          for i in range(size))
        #         for size in self.group_sizes)
        for size in self.group_sizes:
            group = []
            for i in range(size):
                trait_dict = {}
                for trait_name, trait_value in self.traits.items():
                    trait_dict[trait_name] = trait_value.get_variate()
                group.append(Agent(**trait_dict))
            yield group
