Day 1
=====

These were variations of the classic `Two Sum` and `Three Sum` problems. Unlike platforms like [LeetCode](https://leetcode.com) or [HackerRank](https://www.hackerrank.com), Advent of Code only asks you to submit the right answer (pretty much like [Project Euler](https://projecteuler.net)). This means that you can actually get away with brute-force solutions. Despite the fact that the input is quite small, I decided not to go for brute-force.

## Two Sum

We are guaranteed that there is only one combination of two numbers whose sum is our **TARGET_SUM** (2020). The easiest way to find out such combination of numbers is with two nested loops:

```python
for i in range(len(numbers)):
    for j in range(len(numbers)):
        if i != j and i + j == TARGET_SUM:
            return numbers[i], numbers[j]
```

For relatively small inputs (such as the one provided), you can get your answer in no time. This also has the advantage of having O(1) space complexity. When the input grows, however, this solution becomes quite slow. A way of solving this in linear time (at the expense of O(N) space complexity) is by storing the numbers in a data structure that guarantees O(1) time complexity for insertion and lookup, as we go. A set is a good option, since we are only interested in testing membership.

```python
results = set()
for n in numbers:
    x = TARGET_SUM - n
    if x in results:
        return n, x

    results.add(n)
```

Since we are only traversing the collection of numbers once, we achieve a nice linear time complexity.

## Three Sum

This one is also straightforward to solve using a brute-force solution. Instead of two nested loops, you just use three nested loops:

```python
for i in range(len(numbers)):
    for j in range(len(numbers)):
        for k in range(len(numbers)):
            if i != j and i != k and j != k and numbers[i]+numbers[j]+numbers[k] == TARGET_SUM:
                return numbers[i], numbers[j], numbers[k]
```

Again, for relatively small inputs, you can get your answer in no time. But the time complexity is even worse than before, because of the additional nested for loop; its time complexity is O(N^3). Certainly we can do better than that. Fortunately the constraints of the problem are in our favour. For instance, we don't really care much about the order of the tuple of numbers that add up to **TARGET_SUM**: we only care about the numbers. This means that if our input is sorted, it may make things easier when scanning the collection of numbers. The built-in sort functions usually apply an algorithm with a decent time complexity: O(nlogn). Obviously, now that we have the input sorted, we still need to search for the triplet. We can do a linear scan to the array and for each element try and find two other elements that when added together result in **TARGET_SUM**:

```python
def two_sum(numbers, i):
    lo, hi = i+1, len(numbers)-1

    while lo < hi:
        s = numbers[i] + numbers[lo] + numbers[hi]
        if s == TARGET_SUM:
            return numbers[i], numbers[lo], numbers[hi]
        else if s < TARGET_SUM:
            lo += 1
        else:
            hi -= 1

    raise Exception()

def three_sum(numbers):
    numbers = sorted(numbers)

    for i,_ in enumerate(numbers):
        try:
            return two_sum(numbers, i)
        exception:
            pass
```

The idea is to use three pointers in the `two_sum` method and see if the numbers that are indexed by  each pointer add up to the **TARGET_SUM**. If they don't, we just need to check how does that sum compare to the value that we're looking for. Fortunately, we sorted the array beforehand, so we know that if the value we got is lower than the **TARGET_SUM**, we need to search a bit more to the right. If it's higher, we search a bit more to the left. The index `i` is fixed, it only changes on each iteration of the `for` loop in `three_sum`. To summarize, sorting the initial array is an operation that has O(nlogn) time complexity. The `for` loop is O(N^2), because for each element of the array we perform an operation that has linear time complexity. O(N^2 + nlogn) is effectively O(N^2) for very large inputs, which is still way better than O(N^3).
