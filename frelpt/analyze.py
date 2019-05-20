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
        self.add_option_argument("--modules", recursive=True, help="identifier searcher")
        self.add_option_argument("--respaths", recursive=True, help="identifier searcher")
        self.add_option_argument("--invrespaths", recursive=True, help="identifier searcher")

        self.register_forward("trees", help="ASTs used during resolution")
        self.register_forward("modules", help="module ASTs")
        self.register_forward("respaths", help="resolution paths")
        self.register_forward("invrespaths", help="inverted resolution paths")

        self.trees = {}
        self.macros = {}
        self.includes = {}
        self.modules = {}
        self.respaths = {}
        self.invrespaths = {}

    def perform(self, targs):

        if isinstance(targs.trees, dict):
            self.trees.update(targs.trees)
        else:
            import pdb; pdb.set_trace()

        if targs.macros:
            if isinstance(targs.macros, dict):
                self.macros.update(targs.macros)
            else:
                import pdb ;pdb.set_trace()

        if targs.includes:
            if isinstance(targs.includes, dict):
                self.includes.update(targs.includes)
            else:
                import pdb ;pdb.set_trace()

        if isinstance(targs.modules, dict):
            self.modules.update(targs.modules)
        else:
            pass
            #import pdb; pdb.set_trace()

        if targs.respaths:
            if isinstance(targs.respaths, dict):
                self.respaths.update(targs.respaths)
        else:
            pass
            #import pdb; pdb.set_trace()

        if targs.invrespaths:
            if isinstance(targs.invrespaths, dict):
                self.invrespaths.update(targs.invrespaths)
        else:
            pass
            #import pdb; pdb.set_trace()

        searcher_parent = self.get_proxy()
        searcher = Searcher(searcher_parent)

        #resolver = Resolver(trees, macros, includes, insearch_analyzers)
        #argv = [targs.target]
        #direct = FrelptDirective(parent)
        #retval, _forward = direct.run(argv, forward=forward)

        insearch_analyzers = []

        resolver_parent = self.get_proxy()
        resolver = Resolver(resolver_parent)
        resolver_shared = {
            "macros" : self.macros,
            "includes" : self.includes,
            "analyzers" : insearch_analyzers,
            "searcher" : searcher,
            "trees" : self.trees,
        }
        #resolver_parent.shared.update(resolver_shared) 

        analyzer_info = {
            "trees": self.trees,
            "modules": self.modules,
            "respaths": self.respaths,
            "invrespaths": self.invrespaths,
        }

        for node in targs.node:

            topnode = node.topnode()
            filepath = topnode.filepath
            if filepath not in self.trees:
                trees[filepath] = topnode

            searcher_forward = {"node" : node}
            _, _sfwd = searcher.run(["--log", "searcher"], forward=searcher_forward)

            for node, res in _sfwd["ids"].items():

                #if len(analyzer_info["modules"].keys()) == 1: import pdb; pdb.set_trace()

                resolver_forward = {
                    "node" : node,
                    "resolvers" : res,
                    "modules": analyzer_info["modules"],
                    "respaths": analyzer_info["respaths"],
                    "invrespaths": analyzer_info["invrespaths"],
                    "macros" : self.macros,
                    "includes" : self.includes,
                    "analyzers" : insearch_analyzers,
                    "searcher" : searcher,
                    "trees" : analyzer_info["trees"],
                }

                _, rfwd = resolver.run(["--log", "resolver"], forward=resolver_forward)

                analyzer_info["trees"].update(rfwd["trees"])
                analyzer_info["modules"].update(rfwd["modules"])
                analyzer_info["respaths"].update(rfwd["respaths"])
                analyzer_info["invrespaths"].update(rfwd["invrespaths"])

        outsearch_analyzers = []

        for path, tree in self.trees.items():
            for outsearch_analyzer in outsearch_analyzers:
                outsearch_analyzer(path, tree)

        self.add_forward(trees=analyzer_info["trees"])
        self.add_forward(modules=analyzer_info["modules"])
        self.add_forward(respaths=analyzer_info["respaths"])
        self.add_forward(invrespaths=analyzer_info["invrespaths"])
