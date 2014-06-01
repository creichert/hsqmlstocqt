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

ListModel {
    id: model

    function indexOf(date) {
        var end = new Date(stockModel.get(0).date);
        var start = new Date(stockModel.get(stockModel.count() - 1).date);
        if (end <= date)
            return stockModel.count() -1;

        if (start >= date)
            return 0;

        for (var i = 0; i < stockModel.count(); i++) {
            var d = new Date(model.get(i).date);
            if ( d === date)
                return i;
        }
        return -1;
    }

    function requestUrl() {
        if (stockModel.stockId === "")
            return;

        if (stockModel.stockDataCycle !== "d" &&
                stockModel.stockDataCycle !== "w" &&
                stockModel.stockDataCycle !== "m") {
            stockModel.stockDataCycle = "d";
        }

        /*
            Fetch stock data from yahoo finance:
             url: http://ichart.finance.yahoo.com/table.csv?s=NOK&a=5&b=11&c=2010&d=7&e=23&f=2010&g=d&ignore=.csv
                s:stock name/id, a:start day, b:start month, c:start year  default: 25 April 1995, oldest c= 1962
                d:end day, e:end month, f:end year, default:today  (data only available 3 days before today)
                g:data cycle(d daily,  w weekly, m monthly, v Dividend)
          */
        var request = "http://ichart.finance.yahoo.com/table.csv?";
        var startDate = new Date(stockModel.startDate)
        var endDate = new Date(stockModel.endDate)
        request += "s=" + stockModel.stockId;
        request += "&a=" + startDate.getDate();
        request += "&b=" + startDate.getMonth();
        request += "&c=" + startDate.getFullYear();
        request += "&d=" + endDate.getDate();
        request += "&e=" + endDate.getMonth();
        request += "&f=" + endDate.getFullYear();
        request += "&g=" + stockModel.stockDataCycle;
        request += "&ignore=.csv";
        return request;
    }

    function updateStock() {
        var req = requestUrl();

        if (!req)
            return;

        var xhr = new XMLHttpRequest;

        xhr.open("GET", req, true);

        stockModel.ready = false;
        stockModel.clear();
        var i = 1; //skip the first line
        xhr.onreadystatechange = function() {
            if (xhr.readyState === XMLHttpRequest.LOADING || xhr.readyState === XMLHttpRequest.DONE) {
                var records = xhr.responseText.split('\n');

                for (;i < records.length; i++ ) {
                    var r = records[i].split(',');
                    if (r.length === 7)
                        stockModel.append(stockModel.createStockPrice(r[0], r[1], r[2], r[3], r[4], r[5], r[6]));
                }

                if (xhr.readyState === XMLHttpRequest.DONE) {
                    if (stockModel.count() > 0) {
                        stockModel.ready = true;
                        stockModel.stockPrice = stockModel.get(0).adjusted;
                        stockModel.stockPriceDelta = stockModel.count() > 1 ? (Math.round((stockModel.stockPrice - stockModel.get(1).close) * 100) / 100) : 0;
                    }
                }
            }
        }
        xhr.send()
    }
}
