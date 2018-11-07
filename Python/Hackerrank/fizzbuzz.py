#fizzbuzz

def fizzbuzz(n):
    result = []
    for x in range(1, n+1):
        if x % 3 == 0 and x % 5 == 0:
            result.append("fizz buzz")
        elif x % 3 == 0:
            result.append('fizz')
        elif x % 5 == 0:
            result.append('buzz')
        else:
            result.append(str(x))
    return result

def main():
    print(', '.join(fizzbuzz(20)))

main()

####
class Solution(object):
    def fizzBuzz(self, n):
        """
        :type n: int
        :rtype: List[str]
        """
        return [('Fizz' if i % 3 == 0 else '') + ('Buzz' if i % 5 == 0 else '') + (str(i) if not (i % 3 == 0 or i % 5 == 0) else '') for i in range(1,n+1)]