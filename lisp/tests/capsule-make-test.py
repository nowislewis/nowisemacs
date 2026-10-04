"""Check Makefile stage ordering and failure propagation without building packages."""

import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]


class CapsuleMakeTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="capsule-make-")
        self.addCleanup(self.temp.cleanup)
        self.cwd = Path(self.temp.name)
        shutil.copy(ROOT / "Makefile", self.cwd / "Makefile")
        (self.cwd / "lib" / "sample").mkdir(parents=True)
        (self.cwd / "lib" / "not-a-package").write_text("not a directory")
        (self.cwd / "init.org").write_text("dummy")
        self.log = self.cwd / "events"
        self.emacs = self.cwd / "emacs"
        self.args_log = self.cwd / "arguments"
        self.emacs.write_text(
            "#!/usr/bin/env python3\n"
            "import os, sys, time\n"
            "with open(os.environ['CAPSULE_TEST_ARGS'], 'a') as out: out.write(repr(sys.argv) + '\\n')\n"
            "kind = ('autoloads' if any('capsule-batch-prepare' in a for a in sys.argv)\n"
            "        else 'tangle' if any('org-babel-tangle-file' in a for a in sys.argv)\n"
            "        else 'compile')\n"
            "with open(os.environ['CAPSULE_TEST_LOG'], 'a') as log:\n"
            "    log.write(kind + '-start\\n'); log.flush()\n"
            "    if kind == 'autoloads': time.sleep(0.05)\n"
            "    if os.environ.get('CAPSULE_TEST_FAIL') == kind: sys.exit(7)\n"
            "    log.write(kind + '-end\\n')\n"
        )
        self.emacs.chmod(0o755)

    def run_make(self, *targets, fail=""):
        return subprocess.run(
            ["make", "-j8", *targets, f"EMACS={self.emacs}"],
            cwd=self.cwd,
            env={**os.environ, "CAPSULE_TEST_LOG": str(self.log), "CAPSULE_TEST_FAIL": fail,
                 "CAPSULE_TEST_ARGS": str(self.args_log)},
            text=True, capture_output=True,
        )

    @unittest.skipUnless(shutil.which("emacs"), "Emacs is required for stale-bytecode coverage")
    def test_build_and_clean_ignore_stale_capsule_bytecode(self):
        # Exercise real loading: mocks cannot detect Emacs preferring old .elc files.
        emacs = shutil.which("emacs")
        lisp = self.cwd / "lisp"
        lisp.mkdir()
        source = lisp / "capsule.el"
        source.write_text(";;; -*- lexical-binding: t; -*-\n(provide 'capsule)\n")
        compiled = subprocess.run(
            [emacs, "-Q", "--batch", "-f", "batch-byte-compile", str(source)],
            text=True, capture_output=True,
        )
        self.assertEqual(compiled.returncode, 0, compiled.stderr)
        source.write_text(
            ";;; -*- lexical-binding: t; -*-\n"
            "(defun capsule-batch-prepare (&optional native))\n"
            "(defun capsule-batch-compile (directory &optional native))\n"
            "(defun capsule-batch-build-single (package &optional native))\n"
            "(defun capsule-batch-clean ())\n"
            "(provide 'capsule)\n"
        )
        bytecode_time = source.with_suffix(".elc").stat().st_mtime
        os.utime(source, (bytecode_time + 10, bytecode_time + 10))
        for target in ("build", "clean", "lib/sample"):
            with self.subTest(target=target):
                result = subprocess.run(
                    ["make", target, f"EMACS={emacs}"], cwd=self.cwd,
                    text=True, capture_output=True,
                )
                self.assertEqual(result.returncode, 0, result.stderr)

    def test_parallel_compile_waits_for_autoloads(self):
        result = self.run_make("build")
        self.assertEqual(result.returncode, 0, result.stderr)
        events = self.log.read_text().splitlines()
        self.assertEqual(events[:2], ["autoloads-start", "autoloads-end"])
        self.assertEqual(events.count("compile-start"), 2)

    def test_autoload_failure_prevents_compile(self):
        result = self.run_make("build", fail="autoloads")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(self.log.read_text().splitlines(), ["autoloads-start"])

    def test_tangle_failure_is_not_success(self):
        result = self.run_make("init-build", fail="tangle")
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn("init.el generated!", result.stdout)

    def test_build_ignores_non_directory_entries(self):
        result = self.run_make("build")
        self.assertEqual(result.returncode, 0, result.stderr)
        events = self.log.read_text().splitlines()
        self.assertEqual(events.count("compile-start"), 2)  # package plus lisp/
        self.assertEqual(events[-2:], ["tangle-start", "tangle-end"])
        self.assertIn(str(self.cwd), self.args_log.read_text())

    def test_native_mode_covers_packages_and_local_lisp(self):
        result = self.run_make("build", "NATIVE=1")
        self.assertEqual(result.returncode, 0, result.stderr)
        args = self.args_log.read_text()
        self.assertIn('(capsule-batch-compile "lib/sample" t)', args)
        self.assertIn('(capsule-batch-compile "lisp" t)', args)

    def test_single_package_uses_native_flag_without_bulk_prepare(self):
        result = self.run_make("lib/sample", "NATIVE=1")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('(capsule-batch-build-single "sample" t)', self.args_log.read_text())
        self.assertNotIn("autoloads-start", self.log.read_text())


if __name__ == "__main__":
    unittest.main()
