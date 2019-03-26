..  -*- coding: utf-8 -*-


Architecture and design
=========================

Frelpt is constructed as an assembly of multiple modules. Several modules of Frelpt can
be run as an independent tool.


Module-level view
--------------------

    .. figure:: ./_static/frelpt-modules.svg
        :alt: frelpt modules
        :width:  900 px
        :height: 600 px
        :align: center
   
        **Figure:** Frelpt modules



Frelpt modules
--------------------

Frelpt is constructed as an assembly of multiple modules. Modules that are marked with "(APP)" can be executed
as an seperate application.

UI & Sys
~~~~~~~~~~

This module contains a main entry of Frelpt execution and various utilities for other modules. It first
parses arguments provided by user in command-line and configures itself accordingly for next processing.
Once initialized, it invokes `App. build system analyzer(APP)`_ to get macro definitions and include
directories of target application and hand its control over to `Frelpt controller`_.


App. build system analyzer(APP)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This module produces macro definitions and include directories of target application.

**Input**
    * clean command : a Linux command that removes all compiler outputs including object files and mod files
    * build command : a Linux command that compiles source files

**What it does**
    * First execute "clean" command so that "build" command actually compile source files
    * Run "build" command under the control of "strace" Linux utility in order to capture all arguments of compilation.

**Output**
    * macro definitions used during compilation of each source files
    * include directories used during compilation of each source files
 
Inter-procedural dependency analyzer(APP)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This module produces dependency information among all Fortran statements and variables that are required to
implement loop pushdown translation.

**Input**
    * a DO construct node in Abstract Syntax Tree(AST) that will be pushed
    * macro definitions and include directories of files in the target application

**What it does**
    * First, it selects statements in the initial Do loop
    * Select identifiers in the statements
    * Search "definition" statments of the selected identifiers
    * Go to "Select identifer" step with newly found "definition statements"

**Output**
    * a list of ASTs that contains dependency relationship among nodes
 
Function input/output analyzer(APP)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This module marks all dummy and actual arguments of Fortran functions/subroutine as "in", "out", or "inout".

**Input**
    * a list of ASTs that contains dependency relationship among nodes

**What it does**
    * Marks all the dummy arguments with "in", "out", or "inout" according to INTENT attribute or other related specification.
    * Marks all the actual arguments with "in", "out", or "inout" according to the marked dummy arguments.

**Output**
    * a list of ASTs that contains argument nodes that are marked with "in", "out", or "inout"


Frelpt controller
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This module is a main enabler of Frelpt. It drives execution of other modules to create a set of Abstract
Syntax Trees(ASTs) that represents source files modified for loop pushdown translation(LPT).


Directive parser
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This module read Frelpt directive line in source file and retrieve content of the directive.
As of this version, there is only one directive, "pushdown-directive", is defined.

Freplt directive specification version 0.1

    .. productionlist:: freplt-directives
        freplt-directive: pushdown-directive
        pushdown-directive: "!$frelpt" "pushdown"

.. code-block:: fortran
    :linenos:
    :emphasize-lines: 1
    :caption: an example for pushdown directive

    !$frelpt pushdown
    DO i=1, N
        ...
    END DO

Usually, frelpt directive is located just before the statement that is relavant to the directive.
It is possible to have comment and/or blank lines between frelpt directive and the statement.

Type declaration modifier
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This module modifies type declaration in functions and subroutines that are related to loop pushdown translation.
The modification is basically adding one more dimension in array shape.

**Input**
    * a type declaration statement in function or subroutine
    * optionally, the position of the added dimension on array shape

**What it does**
    * modify array dimension definition by adding one more

**Output**
    * a modified statement that one more dimension in array shape is added to


Global variable modifier
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This module modifies type declaration in modules, program, or common block
The modification is basically adding one more dimension in array shape.

**Input**
    * a type declaration statement in modules, program, or common block
    * optionally, the position of the added dimension on array shape

**What it does**
    * modify array dimension definition by adding one more

**Output**
    * a modified statement that one more dimension in array shape is added to


Execution statement modifier
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This module modifies statements in execution part of the progam.
The details of modification are varying according to statement types.
The exact modifications will be defined later.

**Input**
    * T.B.D.

**What it does**
    * T.B.D.

**Output**
    * T.B.D.

Special case modifiers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a collection of modifiers that handle statement modification which does not fit into a regular modifications.
The details of modification are varying according to statement types.
The exact modifications will be defined later.

T.B.D.

AST to source file generator(APP)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This module create files in file system from Abstract Syntax Tree(AST)s

**Input**
    * a list of ASTs

**What it does**
    * Invokes string conversion function on ASTs

**Output**
    * Source files

