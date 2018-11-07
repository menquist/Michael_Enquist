'''
Objective 
In this challenge, we go further with normal distributions. We recommend reviewing the previous challenge's Tutorial before attempting this problem.

Task 
The final grades for a Physics exam taken by a large group of students have a mean of  and a standard deviation of . If we can approximate the distribution of these grades by a normal distribution, what percentage of the students:

Scored higher than  (i.e., have a )?
Passed the test (i.e., have a )?
Failed the test (i.e., have a )?
Find and print the answer to each question on a new line, rounded to a scale of  decimal places.

Input Format

There are  lines of input (shown below):

70 10
80
60
The first line contains  space-separated values denoting the respective mean and standard deviation for the exam. The second line contains the number associated with question . The third line contains the pass/fail threshold number associated with questions  and .

If you do not wish to read this information from stdin, you can hard-code it into your program.

Output Format

There are three lines of output. Your answers must be rounded to a scale of  decimal places (i.e.,  format):

On the first line, print the answer to question  (i.e., the percentage of students having ).
On the second line, print the answer to question  (i.e., the percentage of students having ).
On the third line, print the answer to question  (i.e., the percentage of students having ).
'''
import math

def normal_cdf(x, mean, std_dev):
	return 0.5 * (1 + math.erf((x - mean)/(math.sqrt(2) * std_dev)))

mean, std_dev = list(map(float, input().split()))
q1 = float(input())
q2 = float(input())

print(round( (1-normal_cdf(q1, mean, std_dev))*100, 2))
print(round( (1-normal_cdf(q2, mean, std_dev))*100, 2))
print(round( normal_cdf(q2, mean, std_dev)*100, 2))