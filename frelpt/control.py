# -*- coding: utf-8 -*-

import os

import pyloco

from .direct import FrelptDirective
from .analyze import FrelptAnalyzer
from .translate import FrelptTranslator
from .output import FrelptOutput


class FrelptController(pyloco.PylocoTask):

    def __init__(self, parent):

        self.add_data_argument("target", help="filepath to the source file having 'pushdown' frelpt directive")

        #evaluate=True, parameter_parse=True
        self.add_option_argument("-o", "--outdir", default=os.getcwd(), help="output directory")
        self.add_option_argument("-m", "--macros", help="macro definitions used during compilation for multiple source files")
        self.add_option_argument("-i", "--includes", help="include directories used during compilation for multiple source files")

    def perform(self, targs):

        #######################################
        # argument handling
        #######################################

        macros = {}
        if targs.macros:
            if isinstance(targs.macros, pyloco.Option):
                import pdb ;pdb.set_trace()
            elif isinstance(targs.macros, dict):
                macros.update(targs.macros)
            else:
                import pdb ;pdb.set_trace()

        includes = {}
        if targs.includes:
            if isinstance(targs.includes, pyloco.Option):
                import pdb ;pdb.set_trace()
            elif isinstance(targs.includes, dict):
                includes.update(targs.includes)
            else:
                import pdb ;pdb.set_trace()

        #######################################
        # find frelpt pushdown directive
        #######################################

        forward = {
            "target" : targs.target,
            "macro" : list(macros.get(targs.target, [])),
            "include" : list(includes.get(targs.target, []))
        }

        argv = ["FrelptDirective"]
        direct = FrelptDirective(pyloco.Parent(self, 0))
        retval, _forward = direct.run(argv, forward, {})

        target_tree = _forward["tree"]
        target_donode = _forward["donode"]
        target_subnodes = _forward["subnodes"]

        trees = {targs.target: target_tree}

        #######################################
        # analyze application
        #######################################
        forward = {
            "node" : target_subnodes,
            "macros" : dict(macros),
            "includes" : dict(includes),
            "trees" : trees 
        }

        argv = ["FrelptAnalyze"]
        analyzer = FrelptAnalyzer(pyloco.Parent(self, 0))
        retval, _forward = analyzer.run(argv, forward, {})

        trees = _forward.get("trees", trees)

        import pdb; pdb.set_trace()

        #######################################
        # translate application
        #######################################
        argv = ["FrelptTranslator"]
        trans = FrelptTranslator(pyloco.Parent(self, 0))
        retval, _forward = trans.run(argv, forward, {})

        #######################################
        # generate translated outputs
        #######################################
        argv = ["FrelpOutput"]
        output = FrelptOutput(pyloco.Parent(self, 0))
        retval, _forward = output.run(argv, forward, {})

