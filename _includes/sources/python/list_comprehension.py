import unittest

def evens(min, max):
## SOLUTION_BEGIN
    return [x for x in range(min, max+1) if x % 2 == 0]
## SOLUTION_END

def odds(min, max):
## SOLUTION_BEGIN
    return [x for x in range(min, max+1) if x % 2]
## SOLUTION_END

class Test(unittest.TestCase):
    def test(self):
        self.assertEqual([2, 4, 6, 8, 10], evens(1, 10))
        self.assertEqual([1, 3, 5, 7, 9], odds(1, 10))

if __name__ == "__main__":
    unittest.main()
