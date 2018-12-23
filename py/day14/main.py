from itertools import islice

def sliding_win(it, n):
  buffer = list(islice(it, n - 1))
  for digit in it:
    buffer.append(digit)
    yield buffer
    buffer.pop(0)

def digits(num):
  while True:
    yield num % 10
    num //= 10
    if num == 0:
      break

def bakery_iter():
  i1 = 0
  i2 = 1
  board = [3, 7]
  yield from board
  while True:
    score = board[i1] + board[i2]
    new_items = list(reversed(list(digits(score))))
    board.extend(new_items)
    yield from new_items
    i1 = (i1 + board[i1] + 1) % len(board)
    i2 = (i2 + board[i2] + 1) % len(board)

def format_scores(scores):
  return ''.join(str(score) for score in scores)

def part1(n):
  '''Return next 10 scores after n.'''
  scores = islice(bakery_iter(), n, n + 10)
  return format_scores(scores)

def part2(find_seq):
  '''Return number of recepies before the sequence appears.'''
  seqs = sliding_win(bakery_iter(), len(find_seq))
  for i, seq in enumerate(map(format_scores, seqs)):
    if seq == find_seq:
      return i

if __name__ == '__main__':
  n = 440231
  print(part1(n))
  print(part2(str(n)))
