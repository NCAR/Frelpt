# -*- coding: utf-8 -*-

import os

import pyloco

from .direct import FrelptDirective
from .analyze import FrelptAnalyzer
from .translate import FrelptTranslator
from .output import FrelptOutput


class FrelptController(pyloco.Task):

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
            if isinstance(targs.macros, dict):
                macros.update(targs.macros)
            else:
                import pdb ;pdb.set_trace()

        includes = {}
        if targs.includes:
            if isinstance(targs.includes, dict):
                includes.update(targs.includes)
            else:
                import pdb ;pdb.set_trace()

        #######################################
        # find frelpt pushdown directive
        #######################################

        forward = {
            "macro" : dict(macros.get(targs.target, [])),
            "include" : list(includes.get(targs.target, []))
        }

        parent = self.get_proxy()

        argv = [targs.target]
        direct = FrelptDirective(parent)
        retval, _forward = direct.run(argv, forward=forward)

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

        argv = []
        analyzer = FrelptAnalyzer(parent)
        retval, _forward = analyzer.run(argv, forward=forward)

        #######################################
        # translate application
        #######################################
        forward = {
            "donode" : target_donode,
            "subnodes" : target_subnodes[1:],
            "trees" : _forward["trees"] ,
            "modules" : _forward["modules"], 
            "respaths" : _forward["respaths"], 
            "invrespaths" : _forward["invrespaths"],
            "macros" : macros, 
            "includes" : includes 
        }

        argv = []
        trans = FrelptTranslator(parent)
        retval, _forward = trans.run(argv, forward=forward)

        #######################################
        # generate translated outputs
        #######################################
        forward = {
            "trees" : _forward["trees"] ,
        }

        argv = ["-o", targs.outdir]
        output = FrelptOutput(parent)
        retval, _forward = output.run(argv, forward=forward)
