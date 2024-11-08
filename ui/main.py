import sys
from PySide6.QtWidgets import QApplication, QMainWindow
from PySide6.QtCore import QTimer, QUrl
from reversi_ui import Ui_MainWindow 
from board import ReversiGame
from PySide6.QtMultimedia import QSoundEffect

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
        self.play_effect = QSoundEffect()
        self.play_effect.setSource(QUrl.fromLocalFile("./ui/ui_src/play.wav"))

        # game page buttons
        self.ui.pushButton_new_game.clicked.connect(self.start_new_game)
        self.ui.pushButton_pause.clicked.connect(self.toggle_pause)
        self.pause_effect = QSoundEffect()
        self.pause_effect.setSource(QUrl.fromLocalFile("./ui/ui_src/pause.wav"))
        self.resume_effect = QSoundEffect()
        self.resume_effect.setSource(QUrl.fromLocalFile("./ui/ui_src/resume.wav"))
        
        self.ui.pushButton_quit_game.clicked.connect(self.change_to_main_page)
        self.ui.pushButton_quit_program.clicked.connect(self.quit_program)
        self.general_effect = QSoundEffect()
        self.general_effect.setSource(QUrl.fromLocalFile("./ui/ui_src/click.wav"))

    def initialize_game(self):
        self.game_paused = False
        self.game_time = 0
        self.timer = QTimer()
        self.timer.timeout.connect(self.update_timer)
        self.timer.start(1000)  # Update every second

    def start_new_game(self):
        self.play_effect.play()
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
            self.pause_effect.play()
            self.timer.stop()
            self.ui.pushButton_pause.setText("Resume")
        else:
            self.resume_effect.play()
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
        self.general_effect.play()
        self.ui.stackedWidget.setCurrentIndex(0)

    def quit_program(self):
        self.general_effect.play()
        QTimer.singleShot(200, QApplication.instance().quit) 


if __name__ == "__main__":
    app = QApplication(sys.argv)
    window = MainWindow()
    window.show()
    sys.exit(app.exec())
