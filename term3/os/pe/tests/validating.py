"""Test for the first subtask."""

from .base import TestCaseBase


class ValidatingPeTestCases(TestCaseBase):
    def test_empty_file(self):
        actual = self.run_command(["is-pe", "./tests/1/empty.exe"], 1)
        self.assertEqual(actual, "Not PE\n")

    def test_small_file(self):
        actual = self.run_command(["is-pe", "./tests/1/small.exe"], 1)
        self.assertEqual(actual, "Not PE\n")

    def test_incorrect_signature_1(self):
        actual = self.run_command(["is-pe", "./tests/1/incorrect_signature_1.exe"], 1)
        self.assertEqual(actual, "Not PE\n")

    def test_incorrect_signature_2(self):
        actual = self.run_command(["is-pe", "./tests/1/incorrect_signature_2.exe"], 1)
        self.assertEqual(actual, "Not PE\n")

    def test_incorrect_signature_3(self):
        actual = self.run_command(["is-pe", "./tests/1/incorrect_signature_3.exe"], 1)
        self.assertEqual(actual, "Not PE\n")

    def test_correct(self):
        actual = self.run_command(["is-pe", "./examples/1/1.exe"], 0)
        self.assertEqual(actual, "PE\n")
