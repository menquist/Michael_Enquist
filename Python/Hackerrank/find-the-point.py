# https://www.hackerrank.com/challenges/find-point/problem

for i in range(int(input())):
    px, py, qx, qy = map(int, input().split())
    print ("{0} {1}".format(2*qx-px, 2*qy-py))