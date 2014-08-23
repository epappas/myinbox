'use strict';

var _ = require('lodash');

var async = setImmediate || process.nextTick || setTimeout;

module.exports = ReqTracker;

function ReqTracker(server, logger) {

    server.pre(function (req, res, next) {
        async(function (logger, req) {
            logger.log('verbose', {
                reqState: 'RECEIVED_HEAD',
                env: process.env.NODE_ENV,
                id: req.id(),
                method: req.method,
                url: req.url,
                headers: JSON.stringify(_.omit(req.headers, 'authorization')),
                query: _.omit((typeof req.query === 'function' ? req.query() : req.query ), 'authorization'),
                route: req.route,
                status: 'ok',
                remoteAddress: req.connection.remoteAddress
            });
        }.bind(null, logger, req));
        next();
    });

    server.use(function(req, res, next) {
        async(function (logger, req) {
            logger.log('verbose', {
                reqState: 'RECEIVED_BODY',
                id: req.id(),
                env: process.env.NODE_ENV,
                method: req.method,
                url: req.url,
                query: _.omit((typeof req.query === 'function' ? req.query() : req.query ), 'authorization'),
                params: JSON.stringify(_.omit(req.params, 'authorization')),
                body: JSON.stringify(_.omit(req.headers, 'authorization')),
                status: 'ok'
            });
        }.bind(null, logger, req));
        next();
    });

    server.on('after', function (req, res, route, err) {
        err = err || {};
        async(function (logger, req, res, route, err) {
            logger.log('verbose', {
                reqState: 'RESPONDING',
                env: process.env.NODE_ENV,
                id: req.id(),
                method: req.method,
                url: req.url,
                status: res.statusCode,
                rote: route,
                error: JSON.stringify({message: err.message, stack: err.stack})
            });
        }.bind(null, logger, req, res, route, err));
    });

    server.on('uncaughtException', function (req, res, route, err) {
        err = err || {};
        async(function (logger, req, res, route, err) {
            logger.log('error', {
                reqState: 'EXCEPTION',
                env: process.env.NODE_ENV,
                id: req.id(),
                method: req.method,
                code: err.code || 500,
                url: req.url,
                headers: JSON.stringify(_.omit(req.headers, 'authorization')),
                query: _.omit((typeof req.query === 'function' ? req.query() : req.query ), 'authorization'),
                params: JSON.stringify(_.omit(req.params, 'authorization')),
                route: route,
                error: JSON.stringify({message: err.message, stack: err.stack}),
                status: 'ERROR',
                type: 'uncaught_exception',
                remoteAddress: req.connection.remoteAddress
            });
        }.bind(null, logger, req, res, route, err));

        console.error(err.message, err.stack);

        res.send(err.code || 500, {
            code: err.code || 500,
            error_description: err.status || err.message || err.description || 'Internal Server Error',
            error_uri: '',
            state: req.params.state || req.body.state || undefined
        });
    });

    server.on('UnsupportedMediaType', function (req, res, route, err) {
        err = err || {};
        async(function (logger, req, res, route, err) {
            logger.log('warn', {
                reqState: 'STOPPED',
                env: process.env.NODE_ENV,
                id: req.id(),
                method: req.method,
                code: err.code || 415,
                url: req.url,
                headers: JSON.stringify(_.omit(req.headers, 'authorization')),
                query: _.omit((typeof req.query === 'function' ? req.query() : req.query ), 'authorization'),
                params: JSON.stringify(_.omit(req.params, 'authorization')),
                body: JSON.stringify(_.omit(req.headers, 'authorization')),
                error: JSON.stringify({message: err.message, stack: err.stack}),
                route: route,
                status: 'ERROR',
                type: 'unsupported_mediatype',
                remoteAddress: req.connection.remoteAddress
            });
        }.bind(null, logger, req, res, route, err));

        res.send({
            code: err.code || 415,
            error_description: err.status || err.message || err.description || 'Unsupported media type',
            error_uri: '',
            state: req.params.state || req.body.state || undefined
        });
    });

    server.on('VersionNotAllowed', function (req, res, route, err) {
        err = err || {};
        async(function (logger, req, res, route, err) {
            logger.log('warn', {
                reqState: 'STOPPED',
                env: process.env.NODE_ENV,
                id: req.id(),
                method: req.method,
                code: err.code || 505,
                url: req.url,
                headers: JSON.stringify(_.omit(req.headers, 'authorization')),
                query: _.omit((typeof req.query === 'function' ? req.query() : req.query ), 'authorization'),
                params: JSON.stringify(_.omit(req.params, 'authorization')),
                body: JSON.stringify(_.omit(req.headers, 'authorization')),
                route: route,
                error: JSON.stringify({message: err.message, stack: err.stack}),
                status: 'ERROR',
                type: 'version_not_allowed',
                remoteAddress: req.connection.remoteAddress
            });
        }.bind(null, logger, req, res, route, err));

        res.send({
            code: err.code || 505,
            error_description: err.status || err.message || err.description || 'Version Not Supported',
            error_uri: '',
            state: req.params.state || req.body.state || undefined
        });
    });

    server.on('MethodNotAllowed', function (req, res, route, err) {
        err = err || {};
        async(function (logger, req, res, route, err) {
            logger.log('warn', {
                reqState: 'STOPPED',
                env: process.env.NODE_ENV,
                id: req.id(),
                method: req.method,
                code: err.code || 405,
                url: req.url,
                headers: JSON.stringify(_.omit(req.headers, 'authorization')),
                query: _.omit((typeof req.query === 'function' ? req.query() : req.query ), 'authorization'),
                params: JSON.stringify(_.omit(req.params, 'authorization')),
                body: JSON.stringify(_.omit(req.headers, 'authorization')),
                route: route,
                error: JSON.stringify({message: err.message, stack: err.stack}),
                status: 'ERROR',
                type: 'method_not_allowed',
                remoteAddress: req.connection.remoteAddress
            });
        }.bind(null, logger, req, res, route, err));

        res.send({
            code: err.code || 405,
            error_description: err.status || err.message || err.description || 'Method not allowed',
            error_uri: '',
            state: req.params.state || req.body.state || undefined
        });
    });
}