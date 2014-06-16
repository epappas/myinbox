'use strict';

var express = require('express');
var router = express.Router();
var controller = require('../controllers/index');

module.exports = function(app, redis) {

    router.get('/*', controller(redis).renderIndex)
        .get('/*/*', controller(redis).renderIndex)

    // REGISTER ROUTE 
    // =========================================================================
    app.use('/', router);
};