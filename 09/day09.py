from collections import defaultdict
from circular_list import CircularList

def player_scores(nplayers, nturns):
    scores = defaultdict(int)
    circle = CircularList()
    circle.insert_right(0)
    for marble in range(1, nturns + 1):
        player = marble % nplayers
        if marble % 23 == 0:
            scores[player] += marble
            for _ in range(7):
                circle.move_left()
            scores[player] += circle.delete_move_right()
        else:
            circle.move_right()
            circle.insert_right(marble)
            circle.move_right()
    return scores

def winning_score(nplayers, nturns):
    scores = player_scores(nplayers, nturns)
    return max(scores.values())

if __name__ == '__main__':
    nplayers = 428
    nturns = 70825
    print(winning_score(nplayers, nturns))
    print(winning_score(nplayers, nturns * 100))
