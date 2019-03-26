.. _Frelpt:

Frelpt
==================================

Frelpt is a loop pushdown translation tool written in Python.

Loop pushdown is a type of inter-procedural loop transformations that *pushes* a loop down to terminal
Fortran statements in the bottom of Fortran call stack.

Motivation of this transformation is to transform legacy Fortran code to run fast on modern processors
such Graphical Processing Unit(GPU) and many-core Central Processing Unit(CPU). To be utilized fully, 
the modern processing units require a plenty of parallelization and/or vectorizatin opportunities 
concentrated on a relatively small code region. For example, to utilize GPU well, it is essential
to create a computation intensive kernel and run the kernel on GPU as long as possible.

Loop pushdown effectively transforms a program structure such that loops are moved to the bottom of
Fortran call stack so that computation intensity increases locally at the code regions pointed by the bottom of call stack.


.. toctree::
    :maxdepth: 2

    getting_started
    arch_design.rst

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
