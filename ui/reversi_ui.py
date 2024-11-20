# -*- coding: utf-8 -*-

################################################################################
## Form generated from reading UI file 'reversi.ui'
##
## Created by: Qt User Interface Compiler version 6.6.3
##
## WARNING! All changes made in this file will be lost when recompiling UI file!
################################################################################

from PySide6.QtCore import (QCoreApplication, QDate, QDateTime, QLocale,
    QMetaObject, QObject, QPoint, QRect,
    QSize, QTime, QUrl, Qt)
from PySide6.QtGui import (QBrush, QColor, QConicalGradient, QCursor,
    QFont, QFontDatabase, QGradient, QIcon,
    QImage, QKeySequence, QLinearGradient, QPainter,
    QPalette, QPixmap, QRadialGradient, QTransform)
from PySide6.QtWidgets import (QApplication, QComboBox, QGridLayout, QHBoxLayout,
    QLabel, QMainWindow, QPushButton, QSizePolicy,
    QSpacerItem, QStackedWidget, QVBoxLayout, QWidget)
from PySide6.QtWidgets import QButtonGroup, QRadioButton
import src_rc
from PySide6.QtMultimedia import QSoundEffect

class Ui_MainWindow(object):
    def setupUi(self, MainWindow):
        if not MainWindow.objectName():
            MainWindow.setObjectName(u"MainWindow")
        MainWindow.resize(1500, 750)
        MainWindow.setMinimumSize(QSize(1500, 750))
        MainWindow.setStyleSheet(u"background-color: #fffbe9;")
        
        ## -- Main menu styles -- ##
        
        # Title styles
        widget_game_title_style = """
            background-color: qlineargradient(spread:pad, x1:0, y1:0, x2:1, y2:0, stop:0 rgba(0, 151, 178, 255), stop:1 rgba(126, 217, 87, 255));
            border-radius: 15px;
            border: none;
            text-align: center;
        """
        
        title_style = """
            font-size: 40px;
            text-align: center;
            background-color: transparent;
            font-weight: bold;
        """
        title_flip_style = title_style + """
            color: black;
        """
        title_master_style = title_style + """
            color: white;
        """
        
        icon_style = """
            background-color: transparent; 
            border: none;
            margin: 0 100px;
        """
        
        ## Difficulty level select label styles ##
        level_select_style = """
            color: white;
            font-size: 30px;
            text-align: center;
            background-color: transparent;
            font-weight: bold;
        """
        
        # Game menu style
        game_menu_style = """
            background-color: qlineargradient(spread:pad, x1:0, y1:0, x2:1, y2:0, stop:0 rgba(0, 151, 178, 255), stop:1 rgba(126, 217, 87, 255));
            border-radius: 15px;
            padding: 80px;
            border: none;
            text-align: center;
            margin-top: 15px;
        """
        
        # Difficulty radio button styles
        radio_style = """
            QRadioButton::indicator {
                width: 0px;
                height: 0px;
            }
            QRadioButton::indicator {
                image: url(:/img/ui_src/radio.png);
            }
            QRadioButton:checked {
                background-color: white;
                color: black;
                border: 2px solid black;
            }
            QRadioButton:hover {
                background-color: white;
            }
        """
        
        radio_style_easy = radio_style + """
            QRadioButton {
                background-color: rgb(217, 217, 217);
                color: gray;
                border-radius: 25px;
                padding: 10px 10px 10px 40px;
                test-align: center;
                item-align: center;
                font-size: 22px;
                font-weight: bold;
                border: 2px solid gray;
            }
        """
        
        radio_style_medium = radio_style + """
            QRadioButton {
                background-color: rgb(217, 217, 217);
                color: gray;
                border-radius: 25px;
                padding: 10px 10px 10px 20px;
                test-align: center;
                item-align: center;
                font-size: 22px;
                font-weight: bold;
                border: 2px solid gray;
            }
        """
        
        radio_style_hard = radio_style + """
            QRadioButton {
                background-color: rgb(217, 217, 217);
                color: gray;
                border-radius: 25px;
                padding: 10px 10px 10px 35px;
                test-align: center;
                item-align: center;
                font-size: 22px;
                font-weight: bold;
                border: 2px solid gray;
            }
        """
        
        radio_style_easy = radio_style + """
                QRadioButton {
                        background-color: rgb(217, 217, 217);
                        color: gray;
                        border-radius: 25px;
                        padding: 10px 10px 10px 10px;
                        text-align: center;
                        font-size: 22px;
                        font-weight: bold;
                        border: 2px solid gray;
                }
        """

        radio_style_medium = radio_style + """
                QRadioButton {
                        background-color: rgb(217, 217, 217);
                        color: gray;
                        border-radius: 25px;
                        padding: 10px 10px 10px 15px;
                        text-align: center;
                        font-size: 22px;
                        font-weight: bold;
                        border: 2px solid gray;
                }
        """

        radio_style_hard = radio_style + """
                QRadioButton {
                        background-color: rgb(217, 217, 217);
                        color: gray;
                        border-radius: 25px;
                        padding: 10px 10px 10px 35px;
                        text-align: center;
                        font-size: 22px;
                        font-weight: bold;
                        border: 2px solid gray;
                }
        """

        # Menu buttons
        menu_buttons_style = """
            QPushButton#pushButton_play, QPushButton#pushButton_quit_program {
                background-color: rgb(255, 255, 255);
                color: black;
                border-radius: 30px;
                padding: 10px;
                margin-right: 20px;
                margin-left: 20px;
                font-size: 30px;
                border: none;
                font-weight: bold;
            }
            
            QPushButton#pushButton_play:hover, QPushButton#pushButton_quit_program:hover {
                background-color: rgb(217, 217, 217);
            }
        """
        
        ## -- In game page styles -- ##
        
        # Score widget styles
        score_widget = """
            padding: 20px;
            border: none;
            border-radius: 30px;
            text-align: center;
            color: black;
        """
        
        opponent_score_widget = score_widget + """
            background-color: #d9d9d9;
        """
        
        player_score_widget = score_widget + """
            background-color: #ffcc7f;
        """
        
        # Sidebar menu styles
        buttons_style = """
            QPushButton#pushButton_new_game, QPushButton#pushButton_pause, QPushButton#pushButton_quit_game {
                background-color: rgb(255, 255, 255);
                color: black;
                border-radius: 15px;
                padding: 15px;
                margin: 5px 10px;
                font-size: 20px;
                border: none;
                font-weight: bold;
            }
            QPushButton#pushButton_new_game:hover, QPushButton#pushButton_pause:hover, QPushButton#pushButton_quit_game:hover {
                background-color: rgb(217, 217, 217);
            }
        """
        self.centralwidget = QWidget(MainWindow)
        self.centralwidget.setObjectName(u"centralwidget")
        self.verticalLayout_2 = QVBoxLayout(self.centralwidget)
        self.verticalLayout_2.setObjectName(u"verticalLayout_2")
        self.stackedWidget = QStackedWidget(self.centralwidget)
        self.stackedWidget.setObjectName(u"stackedWidget")
        
        ## -- Main menu page -- ##
        self.page_main_menu = QWidget()
        self.page_main_menu.setObjectName(u"page_main_menu")
        self.verticalLayout_3 = QVBoxLayout(self.page_main_menu)
        self.verticalLayout_3.setObjectName(u"verticalLayout_3")
        self.horizontalLayout_3 = QHBoxLayout()
        self.horizontalLayout_3.setObjectName(u"horizontalLayout_3")
        self.horizontalSpacer_5 = QSpacerItem(40, 20, QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Minimum)

        self.horizontalLayout_3.addItem(self.horizontalSpacer_5)

        self.widget = QWidget(self.page_main_menu)
        self.widget.setObjectName(u"widget")
        self.widget.setMinimumSize(QSize(550, 0))
        self.widget.setMaximumSize(QSize(500, 500))
        self.verticalLayout_4 = QVBoxLayout(self.widget)
        self.verticalLayout_4.setObjectName(u"verticalLayout_4")
        self.horizontalLayout = QHBoxLayout()
        self.horizontalLayout.setObjectName(u"horizontalLayout")
        self.horizontalSpacer = QSpacerItem(40, 20, QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Minimum)

        self.horizontalLayout.addItem(self.horizontalSpacer)

        ## Game title widget ##
        self.widget_game_title = QWidget(self.widget)
        self.widget_game_title.setObjectName(u"widget_game_title")
        self.widget_game_title.setMinimumSize(QSize(530, 100))
        self.widget_game_title.setStyleSheet(widget_game_title_style)
        self.verticalLayout = QVBoxLayout(self.widget_game_title)
        self.verticalLayout.setObjectName(u"verticalLayout")
        self.horizontalLayout_4 = QHBoxLayout()
        self.horizontalLayout_4.setObjectName(u"horizontalLayout_4")
        
        self.white_title = QPushButton(self.widget_game_title)
        self.white_title.setObjectName(u"white_title")
        self.white_title.setStyleSheet(u"border: none;")
        icon = QIcon()
        icon.addFile(u":/img/ui_src/white.png", QSize(), QIcon.Normal, QIcon.Off)
        self.white_title.setIcon(icon)
        self.white_title.setIconSize(QSize(70, 70))
        self.white_title.setStyleSheet(icon_style)
        self.horizontalLayout_4.addWidget(self.white_title)
        
        self.label_title_flip = QLabel(self.widget_game_title)
        self.label_title_flip.setObjectName(u"label_title_flip")
        self.label_title_flip.setStyleSheet(title_flip_style)
        self.label_title_flip.setAlignment(Qt.AlignCenter)

        self.horizontalLayout_4.addWidget(self.label_title_flip)

        self.label_title_master = QLabel(self.widget_game_title)
        self.label_title_master.setObjectName(u"label_title_master")
        self.label_title_master.setStyleSheet(title_master_style)
        self.label_title_master.setAlignment(Qt.AlignCenter)

        self.horizontalLayout_4.addWidget(self.label_title_master)

        self.verticalLayout.addLayout(self.horizontalLayout_4)


        self.horizontalLayout.addWidget(self.widget_game_title)

        self.black_title = QPushButton(self.widget_game_title)
        self.black_title.setObjectName(u"black_title")
        self.black_title.setStyleSheet(u"border: none;")
        icon1 = QIcon()
        icon1.addFile(u":/img/ui_src/black.png", QSize(), QIcon.Normal, QIcon.Off)
        self.black_title.setIcon(icon1)
        self.black_title.setIconSize(QSize(70, 70))
        self.black_title.setStyleSheet(icon_style)
        self.horizontalLayout_4.addWidget(self.black_title)

        self.horizontalSpacer_2 = QSpacerItem(40, 20, QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Minimum)

        self.horizontalLayout.addItem(self.horizontalSpacer_2)


        self.verticalLayout_4.addLayout(self.horizontalLayout)

        self.verticalSpacer = QSpacerItem(20, 40, QSizePolicy.Policy.Minimum, QSizePolicy.Policy.Expanding)

        self.verticalLayout_4.addItem(self.verticalSpacer)
        
        ## Game menu widget ##
        self.widget_game_menu = QWidget(self.widget)
        self.widget_game_menu.setObjectName(u"widget_game_menu")
        self.widget_game_menu.setMinimumSize(QSize(100, 100))
        self.widget_game_menu.setStyleSheet(game_menu_style)
        self.verticalLayout_5 = QVBoxLayout(self.widget_game_menu)
        self.verticalLayout_5.setObjectName(u"verticalLayout_5")
        self.horizontalLayout_2 = QHBoxLayout()
        self.horizontalLayout_2.setObjectName(u"horizontalLayout_2")
        self.horizontalSpacer_3 = QSpacerItem(40, 20, QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Minimum)

        self.horizontalLayout_2.addItem(self.horizontalSpacer_3)

        self.label_level_select = QLabel(self.widget_game_menu)
        self.label_level_select.setObjectName(u"label_level_select")
        self.label_level_select.setStyleSheet(level_select_style)

        self.horizontalLayout_2.addWidget(self.label_level_select)

        self.horizontalSpacer_4 = QSpacerItem(40, 20, QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Minimum)

        self.horizontalLayout_2.addItem(self.horizontalSpacer_4)

        self.verticalLayout_5.addLayout(self.horizontalLayout_2)
        
        self.difficulty_group = QButtonGroup(self.widget_game_menu)
        self.difficulty_group.setObjectName(u"difficulty_group")
        
        # Difficulty radio buttons container
        self.radio_container = QWidget(self.widget_game_menu)
        self.radio_container.setObjectName(u"radio_container")
        self.radio_container.setStyleSheet(u"background-color: transparent; \n margin: 0 10px; ")
        self.radio_layout = QHBoxLayout(self.radio_container)
        self.radio_layout.setSpacing(0)
        self.radio_layout.setObjectName(u"radio_layout")
        
        # Difficulty radio buttons
        self.radio_easy = QRadioButton(self.radio_container)
        self.radio_easy.setObjectName(u"radio_easy")
        self.radio_easy.setText(QCoreApplication.translate("MainWindow", u"Transparent", None))
        self.radio_easy.setStyleSheet(radio_style_easy)
        self.radio_easy.setChecked(True)
        self.radio_easy.setCursor(QCursor(Qt.CursorShape.PointingHandCursor))
        
        self.radio_medium = QRadioButton(self.radio_container)
        self.radio_medium.setObjectName(u"radio_medium")
        self.radio_medium.setText(QCoreApplication.translate("MainWindow", u"MEDIUM", None))
        self.radio_medium.setStyleSheet(radio_style_medium)
        self.radio_medium.setCursor(QCursor(Qt.CursorShape.PointingHandCursor))
        
        self.radio_hard = QRadioButton(self.radio_container)
        self.radio_hard.setObjectName(u"radio_hard")
        self.radio_hard.setText(QCoreApplication.translate("MainWindow", u"HARD", None))
        self.radio_hard.setStyleSheet(radio_style_hard)
        self.radio_hard.setCursor(QCursor(Qt.CursorShape.PointingHandCursor))
        
        self.radio_easy.toggled.connect(self.play_sound_on_toggle)
        self.radio_medium.toggled.connect(self.play_sound_on_toggle)
        self.radio_hard.toggled.connect(self.play_sound_on_toggle)
        
        self.radio_effect = QSoundEffect()
        self.radio_effect.setSource(QUrl.fromLocalFile("./ui/ui_src/click.wav"))
        
        self.radio_layout.addWidget(self.radio_easy)
        self.radio_layout.addWidget(self.radio_medium)
        self.radio_layout.addWidget(self.radio_hard)
        
        self.difficulty_group.addButton(self.radio_easy)
        self.difficulty_group.addButton(self.radio_medium)
        self.difficulty_group.addButton(self.radio_hard)

        self.verticalLayout_5.addWidget(self.radio_container)
        
        # Play and quit buttons
        self.pushButton_play = QPushButton(self.widget_game_menu)
        self.pushButton_play.setObjectName(u"pushButton_play")
        self.pushButton_play.setStyleSheet(menu_buttons_style)
        self.pushButton_play.setCheckable(True)
        self.pushButton_play.setChecked(True)
        self.pushButton_play.setCursor(QCursor(Qt.CursorShape.PointingHandCursor))

        self.verticalLayout_5.addWidget(self.pushButton_play)

        self.pushButton_quit_program = QPushButton(self.widget_game_menu)
        self.pushButton_quit_program.setObjectName(u"pushButton_quit_program")
        self.pushButton_quit_program.setStyleSheet(menu_buttons_style)
        self.pushButton_quit_program.setCursor(QCursor(Qt.CursorShape.PointingHandCursor))

        self.verticalLayout_5.addWidget(self.pushButton_quit_program)

        self.verticalLayout_4.addWidget(self.widget_game_menu)

        self.horizontalLayout_3.addWidget(self.widget)

        self.horizontalSpacer_6 = QSpacerItem(40, 20, QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Minimum)

        self.horizontalLayout_3.addItem(self.horizontalSpacer_6)


        self.verticalLayout_3.addLayout(self.horizontalLayout_3)
        self.stackedWidget.addWidget(self.page_main_menu)
        
        ## -- Game play page -- ##
        self.page_game_play = QWidget()
        self.page_game_play.setObjectName(u"page_game_play")
        self.horizontalLayout_13 = QHBoxLayout(self.page_game_play)
        self.horizontalLayout_13.setObjectName(u"horizontalLayout_13")
        
        ## Sidebar menu widget ##
        self.widget_sidebar_menu = QWidget(self.page_game_play)
        self.widget_sidebar_menu.setObjectName(u"widget_sidebar_menu")
        self.widget_sidebar_menu.setMinimumSize(QSize(200, 450))
        self.widget_sidebar_menu.setMaximumSize(QSize(200, 16777215))
        self.widget_sidebar_menu.setStyleSheet(u"background-color: #b5c7c7; border-radius: 10px;")
        self.verticalLayout_6 = QVBoxLayout(self.widget_sidebar_menu)
        self.verticalLayout_6.setObjectName(u"verticalLayout_6")
        self.horizontalLayout_9 = QHBoxLayout()
        self.horizontalLayout_9.setObjectName(u"horizontalLayout_9")
        
        # Clock timer widget
        self.clocktimer = QPushButton(self.widget_sidebar_menu)
        self.clocktimer.setObjectName(u"clocktimer")
        icon2 = QIcon()
        icon2.addFile(u":/img/ui_src/clock.png", QSize(), QIcon.Normal, QIcon.Off)
        self.clocktimer.setIcon(icon2)
        self.clocktimer.setIconSize(QSize(40, 40))

        self.horizontalLayout_9.addWidget(self.clocktimer)

        self.label_timer = QLabel(self.widget_sidebar_menu)
        self.label_timer.setObjectName(u"label_timer")
        self.label_timer.setStyleSheet(u"font-size: 20px; margin-right: 10px; color: black;")

        self.horizontalLayout_9.addWidget(self.label_timer)

        self.verticalLayout_6.addLayout(self.horizontalLayout_9)
        
        # New game button
        self.pushButton_new_game = QPushButton(self.widget_sidebar_menu)
        self.pushButton_new_game.setObjectName(u"pushButton_new_game")
        self.pushButton_new_game.setStyleSheet(buttons_style)
        self.pushButton_new_game.setCursor(QCursor(Qt.CursorShape.PointingHandCursor))
        self.verticalLayout_6.addWidget(self.pushButton_new_game)
        
        # Pause/Resume button
        self.pushButton_pause = QPushButton(self.widget_sidebar_menu)
        self.pushButton_pause.setObjectName(u"pushButton_pause")
        self.pushButton_pause.setStyleSheet(buttons_style)
        self.pushButton_pause.setCursor(QCursor(Qt.CursorShape.PointingHandCursor))

        self.verticalLayout_6.addWidget(self.pushButton_pause)
        
        # Quit game button
        self.pushButton_quit_game = QPushButton(self.widget_sidebar_menu)
        self.pushButton_quit_game.setObjectName(u"pushButton_quit_game")
        self.pushButton_quit_game.setStyleSheet(buttons_style)
        self.pushButton_quit_game.setCursor(QCursor(Qt.CursorShape.PointingHandCursor))

        self.verticalLayout_6.addWidget(self.pushButton_quit_game)
        self.horizontalLayout_13.addWidget(self.widget_sidebar_menu)
        
        ## Game area widget ##
        self.widget_game_area = QWidget(self.page_game_play)
        self.widget_game_area.setObjectName(u"widget_game_area")
        self.verticalLayout_7 = QVBoxLayout(self.widget_game_area)
        self.verticalLayout_7.setObjectName(u"verticalLayout_7")
        self.horizontalLayout_11 = QHBoxLayout()
        self.horizontalLayout_11.setObjectName(u"horizontalLayout_11")
        self.horizontalSpacer_11 = QSpacerItem(40, 20, QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Minimum)

        self.horizontalLayout_11.addItem(self.horizontalSpacer_11)
        
        # Opponent score widget
        self.widget_opponent = QWidget(self.widget_game_area)
        self.widget_opponent.setObjectName(u"widget_opponent")
        self.widget_opponent.setMaximumSize(QSize(500, 60))
        
        self.widget_opponent.setStyleSheet(opponent_score_widget)
        self.horizontalLayout_7 = QHBoxLayout(self.widget_opponent)
        self.horizontalLayout_7.setObjectName(u"horizontalLayout_7")
        self.horizontalLayout_6 = QHBoxLayout()
        self.horizontalLayout_6.setObjectName(u"horizontalLayout_6")
        self.horizontalSpacer_9 = QSpacerItem(40, 20, QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Minimum)

        self.horizontalLayout_6.addItem(self.horizontalSpacer_9)

        self.pushButton_icon_opponent = QPushButton(self.widget_opponent)
        self.pushButton_icon_opponent.setObjectName(u"pushButton_icon_opponent")
        icon3 = QIcon()
        icon3.addFile(u":/img/ui_src/black.png", QSize(), QIcon.Normal, QIcon.Off)
        self.pushButton_icon_opponent.setIcon(icon3)
        self.pushButton_icon_opponent.setIconSize(QSize(50, 50))

        self.horizontalLayout_6.addWidget(self.pushButton_icon_opponent)

        self.label_opponent_score_2 = QLabel(self.widget_opponent)
        self.label_opponent_score_2.setObjectName(u"label_opponent_score_2")
        self.label_opponent_score_2.setStyleSheet(u"font-size: 25px; margin: 12")

        self.horizontalLayout_6.addWidget(self.label_opponent_score_2)

        self.horizontalSpacer_10 = QSpacerItem(40, 20, QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Minimum)

        self.horizontalLayout_6.addItem(self.horizontalSpacer_10)
        self.horizontalLayout_7.addLayout(self.horizontalLayout_6)
        self.horizontalLayout_11.addWidget(self.widget_opponent)

        self.horizontalSpacer_12 = QSpacerItem(40, 20, QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Minimum)

        self.horizontalLayout_11.addItem(self.horizontalSpacer_12)
        self.verticalLayout_7.addLayout(self.horizontalLayout_11)
        
        ## Game board widget ##
        self.widget_board = QWidget(self.widget_game_area)
        self.widget_board.setObjectName(u"widget_board")
        self.widget_board.setMinimumSize(QSize(500, 500))
        self.gridLayout = QGridLayout(self.widget_board)
        self.gridLayout.setObjectName(u"gridLayout")
        
        # The game board will be added here
        
        self.verticalLayout_7.addWidget(self.widget_board)

        self.horizontalLayout_12 = QHBoxLayout()
        self.horizontalLayout_12.setObjectName(u"horizontalLayout_12")
        self.horizontalSpacer_13 = QSpacerItem(40, 20, QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Minimum)

        self.horizontalLayout_12.addItem(self.horizontalSpacer_13)

        self.widget_opponent_2 = QWidget(self.widget_game_area)
        self.widget_opponent_2.setObjectName(u"widget_opponent_2")
        self.widget_opponent_2.setMaximumSize(QSize(500, 60))
        self.widget_opponent_2.setStyleSheet(player_score_widget)
        self.horizontalLayout_8 = QHBoxLayout(self.widget_opponent_2)
        self.horizontalLayout_8.setObjectName(u"horizontalLayout_8")
        self.horizontalLayout_5 = QHBoxLayout()
        self.horizontalLayout_5.setObjectName(u"horizontalLayout_5")
        self.horizontalSpacer_7 = QSpacerItem(40, 20, QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Minimum)

        self.horizontalLayout_5.addItem(self.horizontalSpacer_7)
        
        # Player score widget
        self.pushButton_icon_player = QPushButton(self.widget_opponent_2)
        self.pushButton_icon_player.setObjectName(u"pushButton_icon_player")
        icon5 = QIcon()
        icon5.addFile(u":/img/ui_src/white.png", QSize(), QIcon.Normal, QIcon.Off)
        self.pushButton_icon_player.setIcon(icon5)
        self.pushButton_icon_player.setIconSize(QSize(50, 50))

        self.horizontalLayout_5.addWidget(self.pushButton_icon_player)

        self.label_player_score = QLabel(self.widget_opponent_2)
        self.label_player_score.setObjectName(u"label_player_score")
        self.label_player_score.setStyleSheet(u"font-size: 25px; margin: 12")

        self.horizontalLayout_5.addWidget(self.label_player_score)
        self.horizontalSpacer_8 = QSpacerItem(40, 20, QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Minimum)
        self.horizontalLayout_5.addItem(self.horizontalSpacer_8)
        self.horizontalLayout_8.addLayout(self.horizontalLayout_5)
        self.horizontalLayout_12.addWidget(self.widget_opponent_2)
        self.horizontalSpacer_14 = QSpacerItem(40, 20, QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Minimum)
        self.horizontalLayout_12.addItem(self.horizontalSpacer_14)
        self.verticalLayout_7.addLayout(self.horizontalLayout_12)
        self.horizontalLayout_13.addWidget(self.widget_game_area)
        self.stackedWidget.addWidget(self.page_game_play)

        self.verticalLayout_2.addWidget(self.stackedWidget)
        MainWindow.setCentralWidget(self.centralwidget)
        self.retranslateUi(MainWindow)
        self.stackedWidget.setCurrentIndex(0)
        QMetaObject.connectSlotsByName(MainWindow)
    # setupUi

    def retranslateUi(self, MainWindow):
        MainWindow.setWindowTitle(QCoreApplication.translate("MainWindow", u"MainWindow", None))
        self.white_title.setText("")
        self.label_title_flip.setText(QCoreApplication.translate("MainWindow", u"Flip", None))
        self.label_title_master.setText(QCoreApplication.translate("MainWindow", u"Master", None))
        self.black_title.setText("")
        self.label_level_select.setText(QCoreApplication.translate("MainWindow", u"Select Your Difficulty", None))
        self.pushButton_play.setText(QCoreApplication.translate("MainWindow", u"PLAY", None))
        self.pushButton_quit_program.setText(QCoreApplication.translate("MainWindow", u"QUIT", None))
        self.clocktimer.setText("")
        self.label_timer.setText(QCoreApplication.translate("MainWindow", u"Time: 00:00", None))
        self.pushButton_new_game.setText(QCoreApplication.translate("MainWindow", u"New Game", None))
        self.pushButton_pause.setText(QCoreApplication.translate("MainWindow", u"Pause", None))
        self.pushButton_quit_game.setText(QCoreApplication.translate("MainWindow", u"Quit", None))
        self.pushButton_icon_opponent.setText("")
        self.label_opponent_score_2.setText(QCoreApplication.translate("MainWindow", u"score", None))
        self.pushButton_icon_player.setText("")
        self.label_player_score.setText(QCoreApplication.translate("MainWindow", u"score", None))
        
    ## Function to play sound when radio button is toggled ##
    def play_sound_on_toggle(self, checked):
        if checked:  # Only play sound when the button is selected
            self.radio_effect.play()