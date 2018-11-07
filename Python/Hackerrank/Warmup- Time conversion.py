'''
Given a time in -hour AM/PM format, convert it to military (24-hour) time.

Note: Midnight is 12:00:00AM on a 12-hour clock, and 00:00:00 on a 24-hour clock. Noon is 12:00:00PM on a 12-hour clock, and 12:00:00 on a 24-hour clock.

Input Format

A single string  containing a time in -hour clock format (i.e.:  or ), where  and .

Output Format

Convert and print the given time in -hour format, where .

Sample Input

07:05:45PM
Sample Output

19:05:45
'''

time = input().strip()
hh = time[0:2]
period = time[-2:]

if period == 'AM':
    if hh == '12':
        output = '00' + time[2:-2]
    else:
        output = time[0:-2]
elif period == 'PM':
    hh = time[0:2]
    if hh == '12':
        output = time[0:-2]
    else:
        output = str(int(hh) + 12) + time[2:-2]
else:
    assert False

print(output)
 
            
    