#!/usr/bin/python3

"""Trait - class for a trait

Takes a function that gets a new value for the trait

"""


class Trait:
    def __init__(self, variate_function):
        self.get_variate = variate_function

    def get_distribution(self, n):
        return (self.get_variate() for i in range(n))
