# Bamboo Garden Trimming Problem
We provide introduction and illustrations for the *Bamboo Garden Trimming
Problem*. We used the treatment by John Kuszmaul [1].

The bamboo trimming problem is a scheduling problem that is simply stated but
not easily solved and with many obvious applications.

## Problem statement
The *Bamboo Garden Trimming Problem* is a resource-augmented, fixed-rate cup
game (see [1] for references).

Let a set of n bamboos {b1, b2, ..., bn} and time intervals ⟦1, 2, ..., t⟧.
At each time intervals, each bamboo grows by a specific rate hi. The rates
are constant throughout time. The rates obey Σhi = 1.

The bamboos start at height 0 and reach heights hi at the end of interval t=1.
At the end of each time interval, a gardener has the possibility to cut down to
zero a single bamboo. The goal is to minimize the maximum height any bamboo has
reached at any point in the process, this quantity is called the *backlog*
(seeing the problem as one of scheduling).

## Problem properties
### Optimal backlog
The optimal backlog is proven to be 2 [1]. The lower bound can be illustrated
by the following resisting example:

Let two bamboos with rates h1 = 1-ε, h2 = ε. The gardener tries to minimize
the backlog by cutting the tallest bamboo at the end of each time interval
(*Reduce-Max* algorithm).

![Lower bound illustration](doc/lower-bound.webm)

The gardener keeps the fastest-growing bamboo in check until he has to skip a
beat to cut the other bamboo. The backlog achieved is 2\*h1 = 2-2\*ε.

## Algorithms
### Listing existing strategies

### Bounds on achieved backlog

## Notation
The noun *bamboo* is generally uncountable, but for the sake of clarity we use
the plural *bamboos*.

## References
1. J. Kuszmaul, “Bamboo Trimming Revisited: Simple Algorithms Can Do Well Too,”
   (2022). https://arxiv.org/abs/2201.07350v1
