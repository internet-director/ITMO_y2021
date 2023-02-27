"""Tests for the third subtask."""

import os
import tempfile
import urllib.request

from .base import TestCaseBase


class ImportFunctionTestCases(TestCaseBase):
    def test_example_1(self):
        actual = self.run_command(["import-functions", "./examples/1/1.exe"], 0)
        expected = self.get_answer("tests/3/1")

        self.assertDictSame(
            self.parse_dict(actual),
            self.parse_dict(expected)
        )

    def test_example_2(self):
        actual = self.run_command(["import-functions", "./examples/2/2.exe"], 0)
        expected = self.get_answer("tests/3/2")

        self.assertDictSame(
            self.parse_dict(actual),
            self.parse_dict(expected)
        )

    def test_example_3(self):
        actual = self.run_command(["import-functions", "./examples/3/3.dll"], 0)
        expected = self.get_answer("tests/3/3")

        self.assertDictSame(
            self.parse_dict(actual),
            self.parse_dict(expected)
        )

    def test_large_exe(self):
        with tempfile.TemporaryDirectory() as directory:
            path = os.path.join(directory, "test.exe")

            urllib.request.urlretrieve("https://nerc.itmo.ru/teaching/os/pe/test.exe", path)
            actual = self.run_command(["import-functions", path], 0)

        expected = self.get_answer("tests/3/4")

        self.assertDictSame(
            self.parse_dict(actual),
            self.parse_dict(expected)
        )
