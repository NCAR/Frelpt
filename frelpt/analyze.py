# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function


import pyloco

from frelpt.fparser_search import Searcher
from frelpt.fparser_resolve import Resolver

class FrelptAnalyzer(pyloco.Task):

    def __init__(self, parent):

        self.add_data_argument("node", nargs="+", help="target nodes")

        self.add_option_argument("-m", "--macros", help="macro definitions used during compilation for multiple source files")
        self.add_option_argument("-i", "--includes", help="include directories used during compilation for multiple source files")
        self.add_option_argument("-t", "--trees", help="a container of ASTs")

        self.register_forward("trees", help="ASTs used during resolution")
        self.register_forward("modules", help="module ASTs")
        self.register_forward("respaths", help="resolution paths")
        self.register_forward("invrespaths", help="inverted resolution paths")

    def perform(self, targs):

        trees = {}
        if isinstance(targs.trees, dict):
            trees.update(targs.trees)
        else:
            import pdb; pdb.set_trace()

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


        parent = self.get_proxy()

        searcher = Searcher(parent)

        #resolver = Resolver(trees, macros, includes, insearch_analyzers)
        #argv = [targs.target]
        #direct = FrelptDirective(parent)
        #retval, _forward = direct.run(argv, forward=forward)

        resolver = Resolver(parent)

        insearch_analyzers = []

        for node in targs.node:

            topnode = node.topnode()
            filepath = topnode.filepath
            if filepath not in trees:
                trees[filepath] = topnode

            searcher_forward = {"node" : node}
            _, _sfwd = searcher.run(["--log", "searcher"], forward=searcher_forward)
            
            for node, res in _sfwd["ids"].items():

                resolver_forward = {
                    "node" : node,
                    "resolvers" : res,
                    "macros" : dict(macros),
                    "includes" : dict(includes),
                    "analyzers" : list(insearch_analyzers),
                    "searcher" : searcher,
                    "trees" : trees,
                }

                _, _rfwd = resolver.run(["--log", "resolver"], forward=resolver_forward)
                trees = _rfwd["trees"]

        modules = _rfwd["modules"]
        respaths = _rfwd["respaths"]
        invrespaths = _rfwd["invrespaths"]

        outsearch_analyzers = []

        for path, tree in trees.items():
            for outsearch_analyzer in outsearch_analyzers:
                outsearch_analyzer(path, tree)

        self.add_forward(trees=trees)
        self.add_forward(modules=modules)
        self.add_forward(respaths=respaths)
        self.add_forward(invrespaths=invrespaths)
