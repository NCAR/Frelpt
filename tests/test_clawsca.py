
import os
import unittest
import tempfile
import shutil
import glob

import pyloco
import frelpt

_here = os.path.dirname(__file__)

claw_sca_tests = "/Users/youngsun/repos/github/claw-compiler/test/claw/sca"

makefile1 = """
fc='gfortran'

org: clean
\t${fc} -o org.exe mo_column.f90 main.f90 && ./org.exe

clean:
\trm -rf *.o *.mod *.exe
"""

makefile2 = """
fc='gfortran'

org: clean
\t${fc} -o org.exe mo_column.f90 mo_column_extra.f90 main.f90 && ./org.exe

clean:
\trm -rf *.o *.mod *.exe
"""

class ClawSCATests(unittest.TestCase):

    def setUp(self):
        self.tempdir = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.tempdir)

    def test_clawsca(self):

        for test in glob.glob(claw_sca_tests + "/sca*"):
            last = os.path.basename(os.path.normpath(test))

            orgdir = os.path.join(self.tempdir, last, "org")
            lptdir = os.path.join(self.tempdir, last, "lpt")

            os.makedirs(orgdir)
            os.makedirs(lptdir)

            makefile = makefile1
            target = os.path.join(test, "main.f90")
            orgmain = os.path.join(orgdir, "main.f90")

            if not os.path.exists(target):
                continue

            with open(target, "r") as fr:
                with open(orgmain, "w") as fw:
                    for line in fr:
                        if line.lstrip().startswith("!$claw sca"):
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

            with self.subTest(sca=last):

                retval, forward = pyloco.perform(frelpt.FrelptTask, argv=[
                    orgmain,
                    "make clean",
                    "make org",
                    "--outdir", lptdir,
                    "--log", "basictests",
                    "--debug",
                ])
         
                out1 = pyloco.system("make run", cwd=orgdir)
                out2 = pyloco.system("make run", cwd=loptdir)

                self.assertEqual(out1[0], 0) 
                self.assertEqual(out2[0], 0) 
                import pdb; pdb.set_trace()
                self.assertEqual(out1[1].split()[-1], out2[1].split()[-1])

            break

test_classes = (ClawSCATests,)

