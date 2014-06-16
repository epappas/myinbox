'use strict';

var fs = require('fs');
var path = require('path');
var express = require('express');
var router = express.Router();
var controller = require('../controllers/index');

module.exports = function(app, redis) {
    router.route('/')
        .get(controller(redis).renderIndex)

    router.route('/partials/:name')
        .get(controller(redis).renderPartials)

    // REGISTER ROUTE 
    // =========================================================================
    app.use('/', router);
};