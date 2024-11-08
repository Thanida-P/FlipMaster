import sys
from PySide6.QtWidgets import QApplication, QMainWindow, QWidget, QLabel, QPushButton, QHBoxLayout, QVBoxLayout
from PySide6.QtCore import QTimer, Qt, QSize, QUrl
from PySide6.QtGui import QPixmap, QCursor
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
        self.board = ReversiGame(self.difficulty, self.result_widget)
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

    def result_widget(self, result):
        overlay = QWidget(parent=self)
        overlay.setGeometry(0, 0, self.width(), self.height())
        overlay.setStyleSheet("background-color: rgba(0, 0, 0, 210);")

        central_widget = QWidget(overlay)
        central_widget.setStyleSheet("background-color: none;")

        label = QLabel(result, parent=central_widget)
        label.setStyleSheet("color: white; font-size: 50px; font-weight: bold;")
        label.setAlignment(Qt.AlignCenter)

        button_layout = QHBoxLayout()
        button_layout.addStretch()
        button_layout.setSpacing(20)
        
        button_style = """
            QPushButton {
                background-color: white; 
                border: 2px solid black;
                border-radius: 30px;
            }
            QPushButton:hover {
                background-color: grey;
            }
            
        """

        # New Game button
        new_game_button = QPushButton("", parent=central_widget)
        new_game_button.setFixedSize(60, 60)
        new_game_pixmap = QPixmap("./ui/ui_src/restart.png")
        new_game_button.setIcon(new_game_pixmap)
        new_game_button.setIconSize(QSize(60, 60))
        new_game_button.setStyleSheet(button_style)
        new_game_button.setCursor(QCursor(Qt.CursorShape.PointingHandCursor))
        new_game_button.clicked.connect(self.start_new_game)
        new_game_button.clicked.connect(self.hide_overlay)
        button_layout.addWidget(new_game_button)
        
        # Home button
        home_button = QPushButton("", parent=central_widget)
        home_button.setFixedSize(60, 60)
        quit_game_pixmap = QPixmap("./ui/ui_src/home.png")
        home_button.setIcon(quit_game_pixmap)
        home_button.setIconSize(QSize(60, 60))
        home_button.setStyleSheet(button_style)
        home_button.setCursor(QCursor(Qt.CursorShape.PointingHandCursor))
        home_button.clicked.connect(self.change_to_main_page)
        home_button.clicked.connect(self.hide_overlay)
        button_layout.addWidget(home_button)

        button_layout.addStretch()

        layout = QVBoxLayout(central_widget)
        layout.addWidget(label)
        layout.addLayout(button_layout)
        layout.setSpacing(20)

        central_widget.setGeometry(
            (self.width() - central_widget.sizeHint().width()) // 2,
            (self.height() - central_widget.sizeHint().height()) // 2,
            central_widget.sizeHint().width(),
            central_widget.sizeHint().height()
        )
        
        self.gameover_effect = QSoundEffect()
        
        if result == "You wins!":
            self.gameover_effect.setSource(QUrl.fromLocalFile("./ui/ui_src/victory.wav"))
        elif result == "You lose!":
            self.gameover_effect.setSource(QUrl.fromLocalFile("./ui/ui_src/gameover.wav"))
        else:
            self.gameover_effect.setSource(QUrl.fromLocalFile("./ui/ui_src/draw.wav"))
        
        self.gameover_effect.play()

        overlay.show()
        
    def hide_overlay(self):
        self.sender().parent().parent().hide()

if __name__ == "__main__":
    app = QApplication(sys.argv)
    window = MainWindow()
    window.show()
    sys.exit(app.exec())
