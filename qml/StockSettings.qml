/****************************************************************************
**
** Copyright (C) 2013 Digia Plc and/or its subsidiary(-ies).
** Contact: http://www.qt-project.org/legal
**
** This file is part of the examples of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:BSD$
** You may use this file under the terms of the BSD license as follows:
**
** "Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions are
** met:
**   * Redistributions of source code must retain the above copyright
**     notice, this list of conditions and the following disclaimer.
**   * Redistributions in binary form must reproduce the above copyright
**     notice, this list of conditions and the following disclaimer in
**     the documentation and/or other materials provided with the
**     distribution.
**   * Neither the name of Digia Plc and its Subsidiary(-ies) nor the names
**     of its contributors may be used to endorse or promote products derived
**     from this software without specific prior written permission.
**
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
** "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
** LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
** A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
** OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
** SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
** LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
** DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
** THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
** (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
** OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
**
** $QT_END_LICENSE$
**
****************************************************************************/

import QtQuick 2.0

Rectangle {
    id: root
    width: 320
    height: 480
    color: "#423A2F"

    Image {
        id: logo
        source: "images/logo.png"
        anchors.horizontalCenter : parent.horizontalCenter
        anchors.top: parent.top
        anchors.topMargin: 15
    }

    Text {
        id: startDateText
        text: "START DATE:"
        color: "#76644A"
        font.pointSize: 15
        anchors.left: parent.left
        anchors.leftMargin: 20
        anchors.top: logo.bottom
        anchors.topMargin: 20
    }

    DatePicker {
        id: startDatePicker
        anchors.left: parent.left
        anchors.leftMargin: 30
        anchors.top: startDateText.bottom
        anchors.topMargin: 8
        onDateChanged: stockModel.startDate = Qt.formatDate(startDatePicker.date, "yyyy-MM-dd")
    }

    Text {
        id: endDateText
        text: "END DATE:"
        color: "#76644A"
        font.pointSize: 15
        anchors.left: parent.left
        anchors.leftMargin: 20
        anchors.top: startDatePicker.bottom
        anchors.topMargin: 20
    }

    DatePicker {
        id: endDatePicker
        anchors.left: parent.left
        anchors.leftMargin: 30
        anchors.top: endDateText.bottom
        anchors.topMargin: 8
        onDateChanged: stockModel.endDate = Qt.formatDate(endDatePicker.date, "yyyy-MM-dd")
    }

    Text {
        id: drawOptionsText
        text: "DRAW OPTIONS:"
        color: "#76644A"
        font.pointSize: 15
        anchors.left: parent.left
        anchors.leftMargin: 20
        anchors.top: endDatePicker.bottom
        anchors.topMargin: 20
    }
    Column {
        id: drawOptions
        anchors.top: drawOptionsText.bottom
        anchors.topMargin: 8
        anchors.left: parent.left
        anchors.leftMargin: 30
        spacing: 2

        Row {
            spacing: 10

            CheckBox {
                id: highButton
                text: "High"
                buttonEnabled: false
                onButtonEnabledChanged: stockSettings.drawHighPrice = highButton.buttonEnabled
            }
            CheckBox {
                id: lowButton
                text: "Low"
                buttonEnabled: false
                onButtonEnabledChanged: stockSettings.drawLowPrice = lowButton.buttonEnabled
            }
        }
        Row {
            spacing: 10
            CheckBox {
                id: openButton
                text: "Open"
                buttonEnabled: false
                onButtonEnabledChanged: stockSettings.drawOpenPrice = openButton.buttonEnabled
            }
            CheckBox {
                text: "Close"
                id: closeButton
                buttonEnabled: true
                onButtonEnabledChanged: stockSettings.drawClosePrice = closeButton.buttonEnabled
            }
        }
        Row {
            spacing: 10
            CheckBox {
                id: volumeButton
                text: "Volume"
                buttonEnabled: true
                onButtonEnabledChanged: stockSettings.drawVolume = volumeButton.buttonEnabled
            }
            CheckBox {
                id: klineButton
                text: "K Line"
                buttonEnabled: false
                onButtonEnabledChanged: stockSettings.drawKLine = klineButton.buttonEnabled
            }
        }
    }

    Text {
        id: chartTypeText
        text: "SHOW PREVIOUS:"
        color: "#76644A"
        font.pointSize: 15
        anchors.left: parent.left
        anchors.leftMargin: 20
        anchors.top: drawOptions.bottom
        anchors.topMargin: 20
    }
    Row {
        anchors.left: parent.left
        anchors.leftMargin: 20
        anchors.top: chartTypeText.bottom
        anchors.topMargin: 8
        spacing: -1
        Button {
            id: yearView
            text: "YEAR"
            buttonEnabled: stockSettings.chartType == "year"
            onClicked: stockSettings.chartType = "year"
        }
        Button {
            id: monthView
            text: "MONTH"
            buttonEnabled: stockSettings.chartType == "month"
            onClicked: stockSettings.chartType = "month"
        }
        Button {
            id: weekView
            text: "WEEK"
            buttonEnabled: stockSettings.chartType == "week"
            onClicked: stockSettings.chartType = "week"
        }
        Button {
            id: allView
            text: "ALL"
            buttonEnabled: stockSettings.chartType == "all"
            onClicked: stockSettings.chartType = "all"
        }
    }

    Component.onCompleted: startDatePicker.date = new Date(1995, 3, 25)
}
