import sys
from PySide6.QtWidgets import QApplication, QMainWindow
from PySide6.QtCore import QTimer
from reversi_ui import Ui_MainWindow 
from board import ReversiGame

class MainWindow(QMainWindow):
    def __init__(self):
        super(MainWindow, self).__init__()
        self.ui = Ui_MainWindow()
        self.ui.setupUi(self)
        self.setup_connections() 
        self.initialize_game()       

    def setup_connections(self):
        # main page buttons
        self.ui.pushButton_play.clicked.connect(self.start_new_game)

        # game page buttons
        self.ui.pushButton_pause.clicked.connect(self.toggle_pause)
        self.ui.pushButton_quit_game.clicked.connect(self.change_to_main_page)
        self.ui.pushButton_quit_program.clicked.connect(self.quit_program)

    def initialize_game(self):
        self.game_paused = False
        self.game_time = 0
        self.timer = QTimer()
        self.timer.timeout.connect(self.update_timer)
        self.timer.start(1000)  # Update every second

    def start_new_game(self):
        if self.ui.radio_easy.isChecked():
            self.difficulty = "easy"
        elif self.ui.radio_medium.isChecked():
            self.difficulty = "medium"
        elif self.ui.radio_hard.isChecked():
            self.difficulty = "hard"
        
        # Create a board
        self.board = ReversiGame(self.difficulty)
        self.ui.gridLayout.addWidget(self.board, 0, 0, 1, 1)
        
        self.ui.stackedWidget.setCurrentIndex(1)
        # Reset game state here
        self.game_time = 0
        # self.update_score_display()
        self.update_timer_display()

    def toggle_pause(self):
        self.game_paused = not self.game_paused
        if self.game_paused:
            self.timer.stop()
            self.ui.pushButton_pause.setText("Resume")
        else:
            self.timer.start()
            self.ui.pushButton_pause.setText("Pause")

    def update_timer(self):
        if not self.game_paused:
            self.game_time += 1
            self.update_timer_display()

    def update_timer_display(self):
        minutes = self.game_time // 60
        seconds = self.game_time % 60
        self.ui.label_timer.setText(f"Time: {minutes:02d}:{seconds:02d}")

    def change_to_main_page(self):
        self.ui.stackedWidget.setCurrentIndex(0)

    def quit_program(self):
        sys.exit()


if __name__ == "__main__":
    app = QApplication(sys.argv)
    window = MainWindow()
    window.show()
    sys.exit(app.exec())
