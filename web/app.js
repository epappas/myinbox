'use strict';

var debug = require('debug')('myinbox');
var express = require('express');
var path = require('path');
var favicon = require('static-favicon');
var logger = require('morgan');
var cookieParser = require('cookie-parser');
var compress = require('compression');
var bodyParser = require('body-parser');
var session = require('express-session')
var RedisStore = require('connect-redis')(session);
var redis = require('redis');
var conf = require('./conf/config');

var app = express();

var redisCon = redis.createClient();

// view engine setup
app.set('env', process.env.NODE_ENV || process.env.MYINBOX_WEB_ENV || 'development');
app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'jade');
app.set('port', process.env.MYINBOX_WEB_PORT || 4242);

app.use(favicon());
app.use(logger('dev'));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded());
app.use(express.static(path.join(__dirname, 'public'))); // to avoid session check upon static serving
app.use(cookieParser());
app.use(session({
    store: new RedisStore({
        host: process.env.MYINBOX_WEB_REDIS_HOST || process.env.REDIS_HOST || 'localhost',
        port: process.env.MYINBOX_WEB_REDIS_PORT || process.env.REDIS_PORT || 6379,
        prefix: 'myinboxweb:ses:',
        ttl: 60 * 60 * 24 // 1 day
    }),
    secret: conf.express.session.secret || 'localsecret'
}))

require('./routes/index')(app, redisCon);
require('./routes/fake-routes')(app, redisCon);
require('./routes/auth')(app, redisCon);

/// catch 404 and forward to error handler
app.use(function(req, res, next) {
    var err = new Error('Not Found');
    err.status = 404;
    next(err);
});

/// error handlers

// development error handler
// will print stacktrace
if (app.get('env') !== 'development') {
    // production error handler
    // no stacktraces leaked to user
    app.use(function(err, req, res, next) {
        console.log(err);
        res.status(err.status || 500);
        res.json({
            status: err.status || 500,
            message: err.message,
            error: { }
        });
    });
}
else {
    app.use(function(err, req, res, next) {
        console.log(err);
        res.status(err.status || 500);
        res.json({
            status: err.status || 500,
            message: err.message,
            error: err
        });
    });
}

module.exports = app;

app.listen(app.get('port'), function() {
    console.log('MyInbox Web server listening on port ' + app.get('port') + ' and PID ' + process.pid);
});

