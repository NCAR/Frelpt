
import os
import unittest
import tempfile
import shutil
import glob
import traceback

import pyloco
import frelpt

_here = os.path.dirname(__file__)

claw_sca_tests = "/Users/youngsun/repos/github/claw-compiler/test/claw/sca"

makefile1 = """
fc='gfortran'

test: clean
\t${fc} -o test.exe mo_column.f90 main.f90 && ./test.exe

clean:
\trm -rf *.o *.mod *.exe
"""

makefile2 = """
fc='gfortran'

test: clean
\t${fc} -o test.exe mo_column.f90 mo_column_extra.f90 main.f90 && ./test.exe

clean:
\trm -rf *.o *.mod *.exe
"""

class ClawSCATests(unittest.TestCase):

    def setUp(self):
        self.tempdir = os.path.realpath(tempfile.mkdtemp())

    def tearDown(self):
        shutil.rmtree(self.tempdir)

    def test_clawsca(self):

        for test in glob.glob(claw_sca_tests + "/sca*"):
            last = os.path.basename(os.path.normpath(test))

            if last in ("sca2", "sca37", "sca38", "sca41", "sca43", "sca45"):
                # sca2 : hung
                # sca37 : not a do-loop
                # sca38 : blank program
                # sca41 : not a do-loop
                # sca43 : not a do-loop
                # sca45 : not a do-loop

                continue

            #if last not in ("sca38",):
            #    continue

            print("FOUND : ", last)

            orgdir = os.path.join(self.tempdir, last, "org")
            lptdir = os.path.join(self.tempdir, last, "lpt")

            os.makedirs(orgdir)
            os.makedirs(lptdir)

            makefile = makefile1
            target = os.path.join(test, "main.f90")
            orgmain = os.path.join(orgdir, "main.f90")

            if not os.path.exists(target):
                print("Test target for " + last + " does not exist.")
                continue

            with open(target, "r") as fr:
                with open(orgmain, "w") as fw:
                    for line in fr:
                        if line.lstrip().startswith("!$claw sca") or line.lstrip().startswith("!$claw parallelize"):
                            fw.write("!$frelpt pushdown\n")
                        else:
                            fw.write(line)

            shutil.copy(os.path.join(test, "mo_column.f90"), orgdir)

            if os.path.isfile(os.path.join(test, "mo_column_extra.f90")):
                makefile = makefile2
                shutil.copy(os.path.join(test, "mo_column_extra.f90"), orgdir)

            with open(os.path.join(orgdir, "Makefile"), "w") as f:
                f.write(makefile)

            with open(os.path.join(lptdir, "Makefile"), "w") as f:
                f.write(makefile)


            # FOR DEBUG: skip pyloco finished test cases
            #if last in ("sca21", "sca19", "sca26"):
            #    continue

#            print("BEGIN : ", last)
#
#            retval, forward = pyloco.perform(frelpt.FrelptTask, argv=[
#                orgmain,
#                "make clean",
#                "make org",
#                "--outdir", lptdir,
#                "--log", "basictests",
#                "--debug",
#            ])
#     
#            print("FINISHED : ", last)
#
#            out1 = pyloco.system("make test", cwd=orgdir)
#            print("ORG EXECUTED : ", last)
#
#            out2 = pyloco.system("make test", cwd=lptdir)
#            print("LPT EXECUTED : ", last)
#
#            self.assertEqual(out1[0], 0) 
#            print("ORG RETURNED NO-ERROR : ", last)
#
#            if out2[0] != 0:
#                print(out2[2])
#                import pdb; pdb.set_trace()
#
#            self.assertEqual(out2[0], 0) 
#            print("LPT RETURNED NO-ERROR : ", last)
#
#            self.assertEqual(out1[1].split()[-1], out2[1].split()[-1])
#            print("PASSED : ", last)
           
            #import pdb; pdb.set_trace()

            with self.subTest(sca=last):

                print("BEGIN : ", last)

                retval, forward = pyloco.perform(frelpt.FrelptTask, argv=[
                    orgmain,
                    "make clean",
                    "make org",
                    "--outdir", lptdir,
                    "--log", "basictests",
                    "--debug",
                ])
         
                print("FINISHED : ", last)

                out1 = pyloco.system("make test", cwd=orgdir)
                print("ORG EXECUTED : ", last)

                out2 = pyloco.system("make test", cwd=lptdir)
                print("LPT EXECUTED : ", last)

                #import pdb; pdb.set_trace()
                self.assertEqual(out1[0], 0) 
                print("ORG RETURNED NO-ERROR : ", last)

                self.assertEqual(out2[0], 0) 
                print("LPT RETURNED NO-ERROR : ", last)

                self.assertEqual(out1[1].split()[-1], out2[1].split()[-1])
                print("PASSED : ", last)
            #break

test_classes = (ClawSCATests,)

