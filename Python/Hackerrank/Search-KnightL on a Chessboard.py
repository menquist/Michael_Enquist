'''

/* KnightL on a Chessboard
 * https://www.hackerrank.com/challenges/knightl-on-chessboard
 * KnightL is a chess piece that moves in an L shape. We define the 
 * possible moves of KnightL(a, b) as any movement from some position 
 * (x1, y1) to some (x2, y2) satisfying either of the following:
 *  1. x2 = x1 ± a and y2 = y1 ± b;
 *  2. x2 = x1 ± b and y2 = y1 ± a;
 * Given the value of 'n' for an 'n x n' chessboard, answer the 
 * following question for each (a, b) pair where 1 <= a, b <= n;
 *  1. What is the minimum number of moves it takes for 
 * KnightL(a, b) to get from position (0, 0) to position (n- 1, n-1)? 
 * If it's not possible for the Knight to reach that destination, 
 * the answer is -1 instead.
 * Then print the answer for each KnightL(a, b) according to the 
 * Output Format specified below.
'''

from collections import deque
n = int(input())
for a in range(1,n):
    row_vals=[]
    for b in range(1,n):
        moves = 0
        board = [[False]*n for _ in range(n)]
        pos_queue=deque([(0,0,0)])
        while pos_queue:
            x,y,depth=pos_queue.pop()
            if not (0<=x<n and 0<=y<n) or board[y][x]:
                continue
            if (x,y)==(n-1,n-1):
                row_vals.append(depth)
                break
            board[y][x]=True
            d=depth+1
            pos_queue.extendleft([(x+a,y+b,d), (x-a,y+b,d), (x+a,y-b,d), (x-a,y-b,d), (x+b,y+a,d), (x-b,y+a,d), (x+b,y-a,d), (x-b,y-a,d)])
        if not pos_queue:
            row_vals.append(-1)
    print(*row_vals)