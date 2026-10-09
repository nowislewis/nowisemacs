"""Exercise package updates with local Git remotes; never update user packages."""

import os
from pathlib import Path
import subprocess
import tempfile
import unittest

SCRIPT = Path(__file__).resolve().parents[2] / "useful-tools/update_submodule.sh"


class CapsuleUpdateTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="capsule-update-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.env = {**os.environ, "GIT_ALLOW_PROTOCOL": "file",
                    "GIT_CONFIG_COUNT": "2", "GIT_CONFIG_KEY_0": "user.name",
                    "GIT_CONFIG_VALUE_0": "Test", "GIT_CONFIG_KEY_1": "user.email",
                    "GIT_CONFIG_VALUE_1": "test@example.com"}
        self.parent = self.root / "parent"
        self.parent.mkdir()
        self.git(self.parent, "init")

    def git(self, cwd, *args):
        result = subprocess.run(["git", "-C", str(cwd), *args], env=self.env,
                                text=True, capture_output=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        return result.stdout.strip()

    def package(self, name, branch=None):
        source = self.root / name
        source.mkdir()
        self.git(source, "init", "-b", "trunk")
        (source / "file").write_text("A")
        self.git(source, "add", ".")
        self.git(source, "commit", "-m", "A")
        if branch:
            self.git(source, "checkout", "-b", branch)
        path = f"lib/{name}"
        self.git(self.parent, "submodule", "add", str(source), path)
        if branch:
            self.git(self.parent, "config", "-f", ".gitmodules",
                     f"submodule.{path}.branch", branch)
        child = self.parent / path
        self.git(child, "checkout", "--detach")
        before = self.git(child, "rev-parse", "HEAD")
        (source / "file").write_text("B")
        self.git(source, "commit", "-am", "B")
        return child, before, self.git(source, "rev-parse", "HEAD")

    def update(self):
        return subprocess.run(["bash", str(SCRIPT)], cwd=self.parent,
                              env=self.env, text=True, capture_output=True)

    def test_detached_head_updates_configured_and_default_branches(self):
        for name, branch in (("explicit", "release"), ("default", None)):
            child, _, target = self.package(name, branch)
            result = self.update()
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertEqual(self.git(child, "rev-parse", "HEAD"), target)
            self.assertEqual(self.git(child, "rev-parse", "--abbrev-ref", "HEAD"), "HEAD")

    def test_failed_package_stops_but_other_package_updates(self):
        bad, before, _ = self.package("bad")
        good, _, target = self.package("good")
        self.git(self.parent, "config", "-f", ".gitmodules",
                 "submodule.lib/bad.branch", "missing")
        result = self.update()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Update failed: lib/bad", result.stdout)
        self.assertEqual(self.git(bad, "rev-parse", "HEAD"), before)
        self.assertEqual(self.git(good, "rev-parse", "HEAD"), target)

    def test_unregistered_directory_is_ignored(self):
        (self.parent / "lib" / "local").mkdir(parents=True)
        (self.parent / ".gitmodules").write_text("")
        self.assertEqual(self.update().returncode, 0)


if __name__ == "__main__":
    unittest.main()
