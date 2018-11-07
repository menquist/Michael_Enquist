'''
At HackerLand University, a passing grade is any grade 40 points or higher on a 100 point scale. Sam is a professor at the university and likes to round each student’s grade according to the following rules:

If the difference between the grade and the next higher multiple of 5 is less than 3, round to the next higher multiple of 5
If the grade is less than 38, don’t bother as it’s still a failing grade
Automate the rounding process then round a list of grades and print the results.

Input Format

First Line

integer 
: number of students

Next  lines

integer 
: individual grades

Output Format

Print  lines, each with the rounded value of a student’s grade in input order.
'''

#!/bin/python3

import sys
import math

def solve(grades):
    # Complete this function
    #toReturn = ""
    for grade in grades:
        newGrade = 0
        #the next higher multiple of 5 is less than 3, round to the next higher multiple of 5
        c = grade % 5
        if(grade >= 38 and 5-c < 3 and c != 0):
            newGrade = grade + 5- c
            if (newGrade > 100):
                newGrade = 100
            print(str(newGrade))
        else:
            newGrade = grade
            print(str(newGrade))


n = int(input().strip())
grades = []
grades_i = 0
for grades_i in range(n):
   grades_t = int(input().strip())
   grades.append(grades_t)
result = solve(grades)     
    