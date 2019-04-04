# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

import os
import pyloco
import subprocess

import fparser.common.readfortran
import fparser.two.parser
import fparser.two.Fortran2003
import fparser.two.utils

from .util import run_shcmd
from .node import nodeclass_template, ConcreteSyntaxNode, BlockNode, Tuple

fparser_do_constructs = (
    fparser.two.Fortran2003.Block_Nonlabel_Do_Construct,
    fparser.two.Fortran2003.Block_Label_Do_Construct,
    fparser.two.Fortran2003.Action_Term_Do_Construct,
    fparser.two.Fortran2003.Outer_Shared_Do_Construct,
    fparser.two.Fortran2003.Block_Do_Construct,
    fparser.two.Fortran2003.Nonblock_Do_Construct,
    fparser.two.Fortran2003.Do_Construct
)

fparser_do_stmts = (
    fparser.two.Fortran2003.Nonlabel_Do_Stmt,
    fparser.two.Fortran2003.Label_Do_Stmt,
    fparser.two.Fortran2003.Do_Term_Action_Stmt,
    fparser.two.Fortran2003.Do_Term_Shared_Stmt,
    fparser.two.Fortran2003.Do_Stmt
)

class Parser(pyloco.PylocoTask):

    def __init__(self, parent):

        self.add_data_argument("target", help="Fortran source input")

        self.add_option_argument("-D", "--macro", nargs="*", help="Fortran macro definition.")
        self.add_option_argument("-I", "--include", nargs="*", help="Fortran source include paths.")
        self.add_option_argument("-S", "--save", action="store_true", help="Save preprocesed file.")
        self.add_option_argument("-a", "--alias", metavar="prefix", nargs="*", help="path alias.")
        self.add_option_argument("-o", "--outdir", default=os.getcwd(), help="output directory")

        self.register_forward("tree", help="Abstract Syntax Tree object")

        self.node_classes = {}

        self.do_constructs = []
        self.do_stmts = []
        self.end_stmts = []

    def gen_aliases(self, paths, aliases):

        aliased_paths = []
        for path in paths:
            for old, new in aliases.items():
                if path.startswith(old):
                    aliased_paths.append(new+"/"+path[len(old):])
                    break
        return aliased_paths

    def perform(self, targs):

        retval = 0
###
        aliases = {}
        if targs.alias:
            for alias in targs.alias:
                if isinstance(alias, pyloco.Option):
                    import pdb ;pdb.set_trace()
                elif isinstance(alias, dict):
                    import pdb ;pdb.set_trace()
                else:
                    import pdb ;pdb.set_trace()

        macros = {}
        if targs.macro:
            for macro in targs.macro:
                if isinstance(macro, pyloco.Option):
                    import pdb ;pdb.set_trace()
                elif isinstance(macro, dict):
                    import pdb ;pdb.set_trace()
                else:
                    import pdb ;pdb.set_trace()

        includes = []
        if targs.include:
            for include in targs.include:
                if isinstance(include, pyloco.Option):
                    includes.extend(include.vargs)
                elif isinstance(include, dict):
                    import pdb ;pdb.set_trace()
                else:
                    import pdb ;pdb.set_trace()
###
#        aliases = {}
#        if targs.alias:
#            for alias in targs.alias:
#                for key, value in alias.kwargs:
#                    aliases[key] = value
#
#        macros = {}
#        if targs.macro:
#            # TODO: need update pyloco to fit options syntax
#            if isinstance(targs.macro, dict):
#                m = targs.macro.get(targs.target, {})
#                macros.update(m)
#            else:
#                for m in targs.macro:
#                    for v in m.vargs:
#                        macros[v] = None
#                    for k, v in m.kwargs.items():
#                        macros[k] = v
#        includes = []
#        if targs.include:
#            for inc in targs.include:
#                for v in inc.vargs:
#                    paths = v.split(":")
#                    apaths = self.gen_aliases(paths, aliases)
#                    includes.extend([s.strip() for s in paths])
#                    includes.extend([s.strip() for s in apaths])

#            if isinstance(targs.include, dict):
#                inc = targs.include.get(targs.target, [])
#                includes.extend(inc)
#            else:
#                for inc in targs.include:
#                    for v in inc.vargs:
#                        paths = v.split(":")
#                        apaths = self.gen_aliases(paths, aliases)
#                        includes.extend([s.strip() for s in paths])
#                        includes.extend([s.strip() for s in apaths])

        try:

            pp = "cpp"
            flags = "-w -traditional -P"

            pack_macros = []
            for k, v in macros.items():
                if v is None:
                    pack_macros.append("-D{}".format(k))
                else:
                    pack_macros.append("-D{0}={1}".format(k,v))

            pack_includes = []
            for p in includes:
                pack_includes.append("-I{}".format(p))

            path = targs.target
            if not os.path.isfile(path):
                apaths = self.gen_aliases([path], aliases)
                if apaths and os.path.isfile(apaths[0]):
                    path = apaths[0]
                else:
                    raise IOError("'%s' does not exist.".format(path))

            with open(path) as fr:
                code = fr.read()
                if type(code) == type(u'A'):
                    code = code.encode('utf-8')
                output, err, retcode = run_shcmd('%s %s %s %s' % (pp, flags, " ".join(pack_includes), " ".join(pack_macros)), input=code)

                if targs.save:
                    root, ext = os.path.splitext(os.path.basename(path))
                    savepath = root+".pre"
                    with open(savepath, 'w') as fw:
                        fw.write(output)

                if type(output) != type(u'A'):
                    output = output.decode('utf-8')



                reader = fparser.common.readfortran.FortranStringReader(output, ignore_comments=False)
                reader.id = path
                parsed = fparser.two.parser.ParserFactory().create(std="f2008")(reader)

                top = ConcreteSyntaxNode(None, [], None)

                stack = [(top, parsed)]

                while stack:

                    parent, node = stack.pop(0)

                    #if node is None or isinstance(node, str):
                    #    continue

                    # groups
                    if node.__class__ in fparser_do_constructs:
                        self.do_constructs.append(node)

                    if node.__class__ in fparser_do_stmts:
                        self.do_stmts.append(node)

                    if isinstance(node, fparser.two.utils.EndStmtBase):
                        self.end_stmts.append(node)

                    children = []

                    if node is not None and not isinstance(node, str):

                        clsname = node.__class__.__name__
                        if clsname not in self.node_classes:

                            genv, lenv = {"BlockNode": BlockNode, "ConcreteSyntaxNode": ConcreteSyntaxNode}, {}
                            if isinstance(node, fparser.two.utils.BlockBase):
                                exec(nodeclass_template.format(clsname=clsname, parentcls="BlockNode"), genv, lenv) 
                            elif isinstance(node, fparser.two.utils.Base):
                                exec(nodeclass_template.format(clsname=clsname, parentcls="ConcreteSyntaxNode"), genv, lenv) 
                            elif clsname in ("tuple", "list"):
                                lenv[clsname] = Tuple
                            else:
                                raise Exception()
                            self.node_classes[clsname] = lenv[clsname]
                        node_class = self.node_classes[clsname]

                        nodetypes = []

                        # node types
                        if isinstance(node, fparser.two.Fortran2003.Comment):
                            nodetypes.append("comment")

                        if hasattr(parent.wrapped, "content"):
                            nodetypes.append("stmt")
                        elif hasattr(parent.wrapped, "items"):
                            nodetypes.append("expr")
                        elif isinstance(parent, Tuple):
                            nodetypes.append("expr")

                        if hasattr(node, "content"):
                            children = node.content
                        elif hasattr(node, "items"):
                            children = node.items
                        elif clsname in ("tuple", "list"):
                            children = node

                        wrapnode = node_class(parent, nodetypes, node)
                        parent.subnodes.append(wrapnode)
                    else:
                        parent.subnodes.append(node)

                    # wrap children
                    for child in children:
                        stack.append((wrapnode, child))

                tree = top.subnodes[0]
                tree.parent = None
                tree.filepath = os.path.abspath(os.path.realpath(path))

                self.add_forward(tree=tree)

                print("Parsed %s"%path)

        except fparser.two.utils.FortranSyntaxError as err:
            print("Parser: FAILED Syntax with '{}'.".format(str(err)))
            retval = -1
        except NameError as err:
            print("Parser: FAILED Name with '{}'.".format(str(err)))
            retval = -2
        except IOError as err:
            print("Parser: FAILED I/O with '{}'.".format(str(err)))
            retval = -3
        except Exception as err:
            print("Parser: FAILED {0} with '{1}'.".format(err.__class__.__name__, str(err)))
            retval = -4

        return retval
