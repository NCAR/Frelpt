# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function


import pyloco

from .fparser_search import Searcher
from .fparser_resolve import Resolver

class FrelptAnalyzer(pyloco.PylocoTask):

    def __init__(self, parent):

        self.add_data_argument("node", nargs="+", help="target nodes")

        self.add_option_argument("-m", "--macros", help="macro definitions used during compilation for multiple source files")
        self.add_option_argument("-i", "--includes", help="include directories used during compilation for multiple source files")
        self.add_option_argument("-t", "--trees", help="a container of ASTs")

        self.register_forward("trees", help="ASTs used during resolution")

    def perform(self, targs):

        trees = {}
        if isinstance(targs.trees, pyloco.Option):
            import pdb; pdb.set_trace()
        elif isinstance(targs.trees, dict):
            trees.update(targs.trees)
        else:
            import pdb; pdb.set_trace()

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

        insearch_analyzers = []
        resolver = Resolver(trees, macros, includes, insearch_analyzers)

        searcher = Searcher()

        for node in targs.node:

            topnode = node.topnode()
            filepath = topnode.filepath
            if filepath not in trees:
                trees[filepath] = topnode

            ids = searcher.run(node)
            for node, res in ids.items():
                resolver.run(node, res)

        outsearch_analyzers = []

        for path, tree in trees.items():
            for outsearch_analyzer in outsearch_analyzers:
                outsearch_analyzer(path, tree)

        self.add_forward(trees=resolver.trees)
