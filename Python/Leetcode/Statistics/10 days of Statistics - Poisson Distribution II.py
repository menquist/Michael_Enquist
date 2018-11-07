import math

val = list(map(float, input().split()))
(a,b) = (val[0], val[1])
print(round(160 + 40*(a + math.pow(a,2)),3))
print(round(128 + 40*(b + math.pow(b,2)),3))

user_input = input()
if user_input.lower() == "end":
    break
MM = float(user_input)
# Do your calculations and print your results