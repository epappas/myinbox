'use strict';

var bunyan = require('bunyan');
var debug = require('debug')('my-api');
var fs = require('fs');
var path = require('path');
var restify = require('restify');
var responseTime = require('response-time');

var conf = require('./config');
var redisTrait = require('./helpers/redis');

var redis = redisTrait.redis;
var server = restify.createServer({
    name: 'my-api',
    log: bunyan.createLogger({
        name: 'audit',
        stream: process.stdout
    }),
    version: conf.get('version')
});

server.use(restify.acceptParser(server.acceptable));
server.use(restify.authorizationParser());
server.use(restify.CORS({
    credentials: true
}));
server.use(restify.queryParser());
server.use(restify.jsonp());
server.use(restify.bodyParser({
    mapParams: true,
    mapFiles: false,
    overrideParams: true,
    keepExtensions: false
}));
server.use(responseTime(0));
server.use(restify.requestLogger());
server.use(restify.throttle({
    burst: 100,
    rate: 50,
    ip: true,
    overrides: {
        '127.0.0.1': {
            rate: 0, // unlimited
            burst: 0
        }
    }
}));
server.use(function (req, res, next) {
    res.header('X-Powered-By', 'MyInbox-API');
    res.header('Server', "api.myinbox.com");
    next();
});

require('./routes/api')(server, redis);


server.on('after', restify.auditLogger({
    log: bunyan.createLogger({
        name: 'audit',
        stream: process.stdout
    })
}));

server.on('uncaughtException', function (req, res, route, err) {
    logger.error('uncaughtException', err.code || 500, route, req.params, err);
    res.send({
        error: err.code || 500,
        error_description: err.status || err.message || err.description || 'Internal Server Error',
        error_uri: '',
        state: req.param.state || req.body.state || undefined
    });
});

server.on('UnsupportedMediaType', function (req, res, route, err) {
    logger.error('UnsupportedMediaType', err.code || 415, route, req.params, err);
    res.send({
        error: err.code || 415,
        error_description: err.status || err.message || err.description || 'Unsupported media type',
        error_uri: '',
        state: req.param.state || req.body.state || undefined
    });
});

server.on('VersionNotAllowed', function (req, res, route, err) {
    logger.error('VersionNotAllowed', err.code || 505, route, req.params, err);
    res.send({
        error: err.code || 505,
        error_description: err.status || err.message || err.description || 'Version Not Supported',
        error_uri: '',
        state: req.param.state || req.body.state || undefined
    });
});

server.on('MethodNotAllowed', function (req, res, route, err) {
    logger.error('MethodNotAllowed', err.code || 405, route, req.params, err);
    res.send({
        error: err.code || 405,
        error_description: err.status || err.message || err.description || 'Method not allowed',
        error_uri: '',
        state: req.param.state || req.body.state || undefined
    });
});

server.listen(conf.get('port'), function () {
    console.log('%s listening at %s', server.name, server.url)
});
