import subprocess
import unittest
from typing import Dict, List, Set


class TestCaseBase(unittest.TestCase):
    def run_command(self, command: List[str], expected_code: int) -> str:
        result = subprocess.run(
            ["./pe-parser"] + command,
            stdout=subprocess.PIPE,
            text=True
        )
        self.assertEqual(expected_code, result.returncode)
        return result.stdout

    def parse_dict(self, data: str) -> Dict[str, Set[str]]:
        content = data.splitlines()

        result = {}
        key = None
        for line in content:
            if line.startswith("    "):
                self.assertFalse(key is None, f"Got function, expected library")
                
                function = line[4:]
                self.assertFalse(function in result[key], f"Duplicating function {function}")
                result[key].add(function)
            else:
                self.assertFalse(line in result, f"Duplicating library {line}")
                key = line
                result[key] = set()

        return result

    def parse_set(self, data: str) -> List[str]:
        result = data.splitlines()
        result_set = set(result)
        self.assertTrue(len(result_set) == len(result), "Duplicating functions")
        return result_set

    def get_answer(self, test: str) -> str:
        with open(test, "r") as f:
            return f.read()

    def assertSetSame(self, actual, expected, prefix=""):
        for key in actual:
            self.assertIn(key, expected, f"{prefix}Unexpected item {key}")
        for key in expected:
            self.assertIn(key, actual, f"{prefix}Missing item {key}")

    def assertDictSame(self, actual, expected):
        self.assertSetSame(actual, expected)
        for key in actual:
            self.assertSetSame(actual[key], expected[key], f"{key}: ")
