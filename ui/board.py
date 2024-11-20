from PySide6.QtWidgets import QGridLayout, QWidget, QLabel
from PySide6.QtCore import Qt, QTimer, QUrl, Signal
from PySide6.QtGui import QPixmap, QCursor, QPainter, QFont, QColor
from pyswip import Prolog
from PySide6.QtMultimedia import QSoundEffect

class ReversiGame(QWidget):
    score_updated = Signal(int, int)
    def __init__(self, difficulty, game_over_callback):
        super().__init__()
        
        self.game_over_callback = game_over_callback
        self.setWindowTitle("Reversi Game")
        self.setFixedSize(500, 500)

        # board
        self.grid_layout = QGridLayout(self)

        self.board_labels = [[None for _ in range(8)] for _ in range(8)]

        self.prolog = Prolog()
        
        self.transparent = False

        if difficulty == "hard":
            self.prolog.consult("alphabetaReversi.pl")
        elif difficulty == "medium":
            self.prolog.consult("minimaxReversi.pl")
        else:
            self.prolog.consult("minimaxReversi.pl")
            self.transparent = True
        
        self.board = None
        self.possibleMoves = None
        self.aiPossibleMoves = None
        self.clear_possible_moves()
        self.playerTurn = True
        self.white = 0
        self.black = 0
        self.sound_effect = QSoundEffect()
        self.sound_effect.setSource(QUrl.fromLocalFile("./ui/ui_src/place.wav"))
        self.start_game_in_prolog()
        self.init_board()
        self.update_possible_moves()

    # initialize/update the board
    def init_board(self):
        for row in range(8):
            for col in range(8):
                label = QLabel("")
                label.setFixedSize(61, 61)
                label.setAlignment(Qt.AlignCenter)
                label.setStyleSheet("border: 1px solid black; background-color: #40bc4f;")
                label.mousePressEvent = lambda event, r=row, c=col: self.handle_move(r, c)

                if self.board[row][col] == 'w':
                    label.setPixmap(QPixmap("./ui/ui_src/white.png").scaled(50, 50, Qt.KeepAspectRatio))
                elif self.board[row][col] == 'b':
                    label.setPixmap(QPixmap("./ui/ui_src/black.png").scaled(50, 50, Qt.KeepAspectRatio))
                else:
                    label.setStyleSheet("border: 1px solid black; background-color: #40bc4f;")

                self.board_labels[row][col] = label
                self.grid_layout.addWidget(label, row, col)
        
        if not self.playerTurn or self.possibleMoves == []:
            QTimer.singleShot(500, self.handleAITurn)
        else:
            self.clear_possible_moves()
            self.update_possible_moves()
            
    # format possible moves to list
    def format_moves(self, moves):
        formatted_moves = []
        for move in moves:
            move = move.strip(',()')
            row, col = map(int, move.split(','))
            formatted_moves.append((row, col))
        return formatted_moves
    
    def format_ai_moves(self, moves_list):
        formatted_moves = []
        for move in moves_list:
            move = move.strip(',').replace(' ', '') 
            row, back = move.split(',,(')
            row = row.strip('(')
            col, score = back.split(',')
            score = score.strip('))')
            formatted_moves.append(((int(row), int(col)), int(score)))
        return formatted_moves

    # start the game
    def start_game_in_prolog(self):
        result = list(self.prolog.query("start_game(Board, UniqueMoves)"))
        
        if result:
            self.board = result[0]["Board"]
            self.possibleMoves = self.format_moves(result[0]["UniqueMoves"])
            self.update_scores()
    
    # clear past possible moves
    def clear_possible_moves(self):
        if hasattr(self, 'board_labels'):
            for row in range(8):
                for col in range(8):
                    if self.board_labels[row][col] is not None:
                        if self.board[row][col] not in ['w', 'b']:
                            self.board_labels[row][col].clear()
                            self.board_labels[row][col].setStyleSheet("border: 1px solid black; background-color: #40bc4f;")
        
    # player move                       
    def handle_move(self, row, col):
        if not self.playerTurn or (row, col) not in self.possibleMoves:
            return
        query = f"play_game({self.board}, w, {row}, {col}, NewBoard, GameOver, White, Black)"
    
        result = list(self.prolog.query(query))
        if result:
            self.white = result[0]["White"]
            self.black = result[0]["Black"]
            if (result[0]["GameOver"] == "true"):
                self.handle_game_over()
                return
            self.board = result[0]["NewBoard"]
            self.playerTurn = False
            self.init_board()
            self.sound_effect.play()
            self.update_scores()
    
    # AI move
    def handleAITurn(self):
        if self.transparent == True:
            query = f"play_game({self.board}, b, NewBoard, UniqueMoves, GameOver, White, Black, Scores)"
        else:
            query = f"play_game({self.board}, b, NewBoard, UniqueMoves, GameOver, White, Black)"
        result = list(self.prolog.query(query))
        
        if result:
            self.white = result[0]["White"]
            self.black = result[0]["Black"]
            if (result[0]["GameOver"] == "true"):

                self.handle_game_over()
                return
            self.board = result[0]["NewBoard"]
            self.possibleMoves = self.format_moves(result[0]["UniqueMoves"])
            if self.transparent == True:
                self.aiPossibleMoves = self.format_ai_moves(result[0]["Scores"])
            self.playerTurn = True
            if self.transparent == True:
                self.update_ai_possible_moves()
                QTimer.singleShot(2000, self.init_board)
                QTimer.singleShot(2000, self.sound_effect.play)
            else:
                self.init_board()
                self.sound_effect.play()
            self.update_scores()
    # game over
    def handle_game_over(self):
        if self.white > self.black:
            result = "You wins!"
        elif self.white < self.black:
            result = "You lose!"
        else:
            result = "It's a tie!"
        if self.game_over_callback:
            self.game_over_callback(result)
    
    # update possible moves
    def update_possible_moves(self):
        for block in self.possibleMoves:
            row, col = block
            label = self.board_labels[row][col]
            disc_pixmap = QPixmap("./ui/ui_src/transparent_disc.png").scaled(50, 50, Qt.KeepAspectRatio)
            label.setPixmap(disc_pixmap)
            label.setStyleSheet("border: 1px solid black; background-color: #40bc4f;")
            label.setCursor(QCursor(Qt.CursorShape.PointingHandCursor)) 
            
        # update ai possible moves
    def update_ai_possible_moves(self):
        for block in self.aiPossibleMoves:
            move, score = block
            row, col = move
            label = self.board_labels[row][col]

            # Load the base pixmap
            disc_pixmap = QPixmap("./ui/ui_src/transparent_disc_ai.png").scaled(50, 50, Qt.KeepAspectRatio)

            # Create a painter to overlay the score
            painter = QPainter(disc_pixmap)
            painter.setFont(QFont("Arial", 10, QFont.Bold))  # Customize font size and style
            painter.setPen(QColor("black"))  # Set text color
            painter.drawText(disc_pixmap.rect(), Qt.AlignCenter, str(score))  # Draw score at the center
            painter.end()

            # Set the modified pixmap to the label
            label.setPixmap(disc_pixmap)
            label.setStyleSheet("border: 1px solid black; background-color: #40bc4f;")
            label.setCursor(QCursor(Qt.CursorShape.PointingHandCursor))

    # count the number of pieces
    def count_pieces(self):
        white_count = 0
        black_count = 0
        for row in self.board:
            white_count += row.count('w')
            black_count += row.count('b')
        return white_count, black_count

    # update the scores
    def update_scores(self):
        white_count, black_count = self.count_pieces()
        self.white = white_count
        self.black = black_count
        self.score_updated.emit(self.white, self.black) 
        
    # reset the board
    def reset_board(self):
        self.get_board_and_moves()
        self.init_board()