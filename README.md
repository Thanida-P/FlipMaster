# FlipMaster
FlipMaster was developed as part of our artificial intelligence class during our first semester of our 3rd year. It is an innovative strategy board game designed around the classic game of Reversi. This two-player game is played on an 8x8 uncheckered board using double-sided discs, with each player taking turns to
place their discs strategically. The objective is to capture the opponent’s discs by sandwiching them between their own. As the game progresses and the
board fills up, the player with the most discs of their color at the end wins.

FlipMaster integrates artificial intelligence into its gaming mechanism,
providing a challenging opponent for players who wish to enhance their
Reversi skills.

<b>Utilized Technologies</b>

1. Prolog: Prolog is implemented to establish the rules of a game where
players can flip their opponent's pieces by trapping them between two
of their own. It will facilitate the determination of legal moves and the
flipping of pieces. Additionally, Prolog will incorporate AI strategies
that prioritize moves located near the edges or corners of the game
board.

2. Python: The implementation of Python will focus on developing a user interface that displays the game board and its pieces, facilitating gameplay against the AI. Additionally, Python will handle turn-based interactions and provide a visual representation of the game's progression.

<b>The AI Concepts to Apply in this Project</b>

1. Search Algorithm: In this project, we focus on a search algorithm that evaluates potential moves while also considering the future consequences of those moves. The aim is to make decisions rooted in an optimal game strategy, such as a particular emphasis on prioritizing edges and corners. Two primary search algorithms have been implemented including:
- Minimax Algorithm: The AI will choose the move that leads to the highest possible score, by assuming the opponent is trying to minimize the AI’s score

- Alpha-Beta Pruning: The AI will still make optimal moves similar to the minimax algorithm but with improved performance, particularly when facing large decision trees in complex board states

2. The AI employs heuristics to assess non-terminal game states, where a definitive winner or loser has not yet been determined. A heuristic function assigns a value to each board state, indicating how favorable it is for the AI. The AI uses this value to decide which move to make.
